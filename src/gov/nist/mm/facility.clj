(ns dtype    (:use [tawny.owl]))
(ns vaem     (:use [tawny.owl]))
(ns qudt     (:use [tawny.owl]))
(ns modeling (:use [tawny.owl]))

(ns operations
  (:use [tawny.owl])
  (:require [dtype]
            [vaem]
            [qudt]
            [modeling]
            #_[tawny.reasoner]))

(ns gov.nist.mm.facility
  (:require [immutant.web             :as web]
            [immutant.web.async       :as async]
            [immutant.web.sse         :as sse]
            [immutant.web.middleware  :as immutant]
            [compojure.route          :as route]
            [compojure.core     :refer (ANY GET defroutes)]
            [ring.util.response :refer (response redirect content-type)]
            [ring.util.codec    :refer (url-encode url-decode)]
            [environ.core       :refer (env)]
            [hiccup.core :refer (html)]
            [cheshire.core]
            [clojure.string :as str]
            
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [edu.ucdenver.ccp.kr.jena.kb :as jena]
            
            [tawny.owl :as tawny.owl]
            [tawny.query :as query]
            [tawny.render]
            [tawny.read :as rowl]
            [tawny.lookup :as look]

            [gov.nist.mm.util :refer :all])
  (:use clojure.java.io)
  (:import
   (org.semanticweb.owlapi.model
    HasIRI
    OWLAxiom
    OWLEntity
    OWLObject
    OWLOntologyManager OWLOntology IRI
    OWLClassExpression OWLClass OWLAnnotation
    OWLDataProperty OWLObjectProperty
    OWLDataPropertyExpression ; not useful?
    OWLIndividual OWLDatatype
    OWLObjectPropertyExpression
    OWLNamedObject OWLOntologyID)
   [org.semanticweb.owlapi.search EntitySearcher])
  (:import java.net.URI
           java.io.ByteArrayInputStream)
  (:gen-class))

(def rrr "temporary, for debugging" (atom nil))


(def ontos "A vector of the ontologies descriptionsused in this project"
  {:dtype {:ns (find-ns 'dtype)
           :iri "http://www.linkedmodel.org/schema/dtype"
           :file "dtype.owl"}
   :vaem  {:ns (find-ns 'vaem)
           :iri "http://www.linkedmodel.org/schema/vaem"
           :file "vaem.owl"}
;   :qudt  {:ns (find-ns 'qudt)
;           :iri "http://qudt.org/2.0/schema/qudt"
;           :file "qudt.ttl"}
   :modeling {:ns (find-ns 'modeling)
              :iri "http://modelmeth.nist.gov/modeling"
              :file "modeling.ttl"}
   :operations {:ns (find-ns 'operations)
                :iri "http://modelmeth.nist.gov/operations"
                :file "operations.ttl"}})

;;; This should be elsewhere. Probably a separate project shared by facility and ontolatex. 
;;; ================= Tawny stuff borrowed from ontolatex ====================

;;; Concepts:
;;; - Ontologies are associated with namespaces. See ref @tawny.owl/ontology-for-namespace.
;;; - You need to be in the namespace to do the operations. (tawny.owl/get-current-ontology)

(def tawny-types [:tawny.owl/class :tawny.owl/individual :tawny.owl/property
                  :tawny.owl/object-property :tawny.owl/data-property])


;;; POD Useless? 
(def manager (tawny.owl/owl-ontology-manager))

(def project-ontos
  (map #(OWLOntologyID. (tawny.owl/iri %)) ontologies))

;;; POD I expected this to be http://modelmeth.nist.gov/modeling#clojureCodeNote
(def ^:const code-iri "Used to identify clojure notes from thing-mapped objects."
  (list :iri "http://modelmeth.nist.gov/modeling#clojureCode"))
                   
(defn load-onto
  "Load an ontology. The argument is a keyword from the ontos map."
  [onto]
  (binding [*ns* (-> ontos onto :ns)]
    (tawny.owl/remove-ontology-maybe
     (OWLOntologyID. (IRI/create (-> ontos onto :iri))))
    (dosync
     (alter tawny.owl/ontology-for-namespace
            #(assoc %
                    (-> ontos onto :ns)
                    (.loadOntologyFromOntologyDocument
                     (tawny.owl/owl-ontology-manager)
                     (IRI/create (clojure.java.io/resource (-> ontos onto :file)))))))))

(defn load-ontos
  "Load all the project ontologies."
  []
  (map load-onto (keys ontos))
  #_(binding [*ns* (-> ontos :operations :ns)] 
    (tawny.owl/owl-import (-> ontos :modeling :ns)))) ; <================= WRONG!
    
(defn onto-ref
  "Safely dereference a symbol to a ontology entry."
  [sym]
  (when-let [var (resolve sym)]
    (var-get var)))
  
(declare thing-map)

(defn clojure-code
  "Return any http://modelmeth.nist.gov/modeling#clojureCode annotation"
  [obj]
  (some #(when (and (= (:otype %) :annotation)
                    (= (:type %) code-iri))
           (:literal %))
        (-> obj thing-map :notes)))

;;; POD needs to return true if a supertype is ignored. 
(defn ignore?
  "Returns true if the tawny thing has a clojure {:priority :ignore}"
  [obj]
  (if (some #(= (tawny.owl/guess-type (tawny.owl/get-current-ontology) obj) %) tawny-types)
    (when-let [code (clojure-code obj)]
      (= :ignore (:priority (read-string code))))
    true))

(defn onto-parent-child-map
  "Define the parent/child relationship as a map."
  [root]
  (let [obj2var-map
        (let [ks (remove #(ignore? (var-get %))
                         (vals (ns-interns onto-ns)))
              vs (map var-get ks)]
          (clojure.set/map-invert (zipmap ks vs))),
        m (reduce (fn [index node]
                    (assoc index (get obj2var-map node)
                           (map #(get obj2var-map %)
                                (tawny.owl/direct-subclasses node))))
                  {}
                  (conj (tawny.owl/subclasses root) root))]
    (as-> m ?map
      (dissoc ?map nil)
      (reduce (fn [m k] (update m k #(vec (filter identity %))))
              ?map
              (keys ?map)))))

(defn next-paths
  "Return all paths one-step further than the argument, if any."
  [path index]
  (if (empty? path)
    []
    (vec (map #(conj path %) (vec (get index (last path)))))))

(defn onto-root-map
  "Define the ontology root structure as a nested map."
  [accum paths index]
  (if (empty? paths)
    accum
    (recur
     (assoc-in accum (first paths) {})
     (let [nexts (next-paths (first paths) index)]
       (if (empty? nexts)
         (vec (next paths))
         (into nexts (vec (next paths)))))
     index)))

(defn vectify
  "Turn a nested map like that from onto-root-map into a nested vector
   where every var leaf is followed by a vector representing its subclasses 
   (could be empty)."
  [nested-map]
  (clojure.walk/prewalk
   #(if (var? %)
      %
      (vec (interleave (keys %) (vals %))))
   nested-map))

(defn roots-nested-vector
  "Return a nested vector of root objects and their subclasses."
  [& root-concepts]
  (map (fn [root-sym]
         (let [pc-map (onto-parent-child-map (var-get (resolve root-sym)))]
           (-> (onto-root-map {} [[(resolve root-sym)]] pc-map) 
               vectify )))
       root-concepts))

(defn tryme []
  (roots-nested-vector 'onto/OperationsDomainConcept)
  (tawny.owl/subclasses (var-get (resolve 'onto/OperationsDomainConcept))))
;;; ================= End  ====================


;;; TODO:
;;;   DONE - Try to remove package specification for hiccup.
;;;   DONE - Find Page Design css etc.

;;; http://stackoverflow.com/questions/3644125/clojure-building-of-url-from-constituent-parts
(defn make-query-string [m & [encoding]]
  (let [s #(if (instance? clojure.lang.Named %) (name %) %)
        enc (or encoding "UTF-8")]
    (->> (for [[k v] m]
           (str (url-encode (s k) enc)
                "="
                (url-encode (str v) enc)))
         (interpose "&")
         (apply str))))

(defn build-url [url-base query-map & [encoding]]
  (str url-base "?" (make-query-string query-map encoding)))

(defn echo
  "Echos the request back as a string."
  [request]
  (-> (response (with-out-str (pprint request)))
    (content-type "text/plain")))

(defn wrap-content-type 
  "Add to headers on response. (See ring-clojure/ring 'Concepts' page on GitHub.)"
  [handler content-type]
  (fn [request]
    (let [response (handler request)]
      (assoc-in response [:headers "Content-Type"] content-type))))

(defn- active-tab 
  [ & {:keys [tab] :or {tab :system}}]
  "Return HTML div metaltop-teal tabs, marking TAB as active."
  `[:ul
    ~@(map (fn [[k v]]
             (if (= k tab)
               `[:li {:class "active"} [:a {:href ~(str "/FacilitySearch/" (name k))} ~v]]
               `[:li                   [:a {:href ~(str "/FacilitySearch/" (name k))} ~v]]))
           {:system "Production System",
            :modeling "Modeling"
            :concepts "Concepts"
            :search "Search"})])

;;; ToDo:
;;;   - Add parameters (title, at least. See pod-utils/html-utils)
;;;   - Add a real style sheet (which could be a parameter).
(defmacro app-page-wrapper
  "Wrap pages with stylesheet, start session, etc."
  [args & body]
  `(->
    (response 
     (html [:html {:lang "en"}
            [:head [:title "Production Facility"]
             [:meta {:http-equiv "content-type" :content "text/html" :charset="iso-8859-1"}]
             [:link {:rel "stylesheet" :type "text/css" :href "/style.css"}]] ; Really need /style.css STRANGE!
            [:div {:id "metaltop-teal"} ~(active-tab :tab (:tab args))]
            [:body ~@body]]))
    (content-type "text/html")))

(defn empty-of 
  [s]
  "Return an empty copy of type S."
  (cond (vector? s) []
        (list? s) '()
        (map? s) {}
        (set? s) #{}))

(defn seq-equal-length
  "Return vectors or lists of equal length, adding elements ELEM to end of shorter, if necessary."
  [l1 l2 elem]
  (let [c1 (count l1)
        c2 (count l2)
        make (if (vector? l1) vec list*)] 
    (cond (= c1 c2) (vector l1 l2)
          (> c1 c2) (vector l1 (make (concat l2 (into (empty-of l2) (repeatedly (- c1 c2) (fn [] elem))))))
          (< c1 c2) (vector    (make (concat l1 (into (empty-of l1) (repeatedly (- c2 c1) (fn [] elem))) l2))))))

;;; POD Use url-params http://briancarper.net/clojure/compojure-doc.html
(defn owl-class-url
  [obj & {:keys [root] :or {root "concepts"}}]
  "Return a URL for an owl:Class."
  `[:a {:href ~(str root "?name=" (url-encode obj))} ~(str (name obj))])

(defn- system-pg
  "Handle page for system tab."
  [request]
  #_(app-page-wrapper {:tab :system}
    (let [params (sparql/query mfg-kb '((?/x rdfs/subClassOf Physical)))
          params-url (map owl-class-url (map '?/x params))]
      (html
        [:h1 "Production System Concepts"]
        `[:ul
          ~@(map (fn [d1] [:li d1]) params-url)]))))

(defn- modeling-pg
  "Handle page for facility modeling tab."
  [request]
  (app-page-wrapper {:tab :modeling}
   "Modeling: Nothing here yet."))

(defn- search-pg
  "Handle page for search tab."
  [request]
  (app-page-wrapper {:tab :search}
                    "All: Nothing here yet."))

(defn page-tab
  "Return the keyword indicating the tab on which the page should be displayed."
  [request & {:keys [default] :or {default :system}}]
  (if-let [tab (:tab (:params request))]
    (keyword tab)
    default))

(defn lookup-concept
  "Return a map of everything we know about whatever."
  [name]
  :nyi)

(defn simplify-tawny-annotations
  "Some are (:comment <pairs>). Some are (:annotation code-iri <pairs>)"
  [tawny-notes]
  (vec
   (doall
    (map #(let [original %]
            (as-> original ?note
              (cond (= (first ?note) :comment) (second ?note),
                    (= (first ?note) :annotation ) (-> ?note rest rest first))
              (apply hash-map ?note)
              (assoc ?note :otype (first original))))
         tawny-notes))))

(defn short-name
  "Argument is an OWLClassImpl etc."
  [obj]
  (->> obj
       look/named-entity-as-string
       (re-matches #".*\#(.*)")
       second))

(defn properties-of
  "Return a seq of properties that are relevant to the argument tmap."
  [tmap property-map]
  (let [sname (:short-name tmap)]
     (filter (fn [pm] (or (some #(= sname %) (:domains pm))
                          (some #(= sname %) (:ranges  pm))))
             (vals property-map))))

(defn thing-map
  "Return a map of information about the class. See also query/into-map-with"
  ([obj] (thing-map obj {})) ; Used by ignore? 
  ([obj property-map]
   (when (instance? OWLClass obj)
     (let [sname (short-name obj)]
       (as-> (apply hash-map (tawny.render/as-form obj :keyword true)) ?map
         (assoc ?map :short-name sname) ; POD (or label)
         (assoc ?map :var (intern onto-ns (symbol sname)))
         (assoc ?map :notes (simplify-tawny-annotations (:annotation ?map)))
         (assoc ?map :subclass-of (doall (map short-name ; POD there are other ways. See notes 2017-07-22. 
                                              (filter #(instance? OWLClass %)
                                                      (tawny.owl/direct-superclasses obj)))))
         (assoc ?map :properties (properties-of ?map property-map)))))))

(defn- concept-desc-pg
  "Describe an owl:Class."
  [request]
  (reset! rrr request)
  (app-page-wrapper {:tab :concepts #_(page-tab request)}
                    (str "Got it: params = " (:params request) " "
                         "query-string = "
                         (when-let [q (:query-string request)]
                           (url-decode q)))))

(defroutes routes
  (GET "/" [] system-pg)
  (GET "/FacilitySearch/:tab" [tab] 
       (cond (= tab "system") system-pg
             (= tab "modeling"  ) modeling-pg
             (= tab "concepts") concept-desc-pg
             (= tab "search" ) search-pg))
  (GET "/concepts*" [tab] concept-desc-pg) ; POD Hmmm...
  (route/resources "/") ; This one gets used for static pages in resources/public
  (ANY "*" [] echo)) ; Good for diagnostics

;;; ============== Server Management ======================================================
(defn -main [& {:as args}]
  (web/run
    (-> routes
      (immutant/wrap-session {:timeout 20})
      (immutant/wrap-development)) ; POD added - saw it in the source, looked interesting.
    (merge {"host" (env :demo-web-host), "port" (env :demo-web-port)}
           args)))

;;; (Re)start the server
(defn restart []
 (web/stop (-main :port 3034))
  (-main :port 3034))

;;;================== /FacilitySearch/concepts?name=foo =====================================
;;; POD rewrite with reduce
(defn- table-vals
  [text]
  "Returns vector of table rows (list of the row's elements) if TEXT (vector of strings) start a table."
  (when (re-matches #"^\s*\|[-,:,\|]+\|.*\n$" (text 0)) ; second line of markdown table is just |------|
    (let [len (dec (count text))]
      (loop [line-num 1
             rows []] ; odd-looking because only want first table in this cell (if many).
        (if (and (< line-num len) 
                 (re-matches #"\s*\|.*\|\n$" (text line-num)))
          (recur
           (inc line-num)
           (conj rows (butlast (rest (str/split (text line-num) #"\|")))))
          rows)))))

