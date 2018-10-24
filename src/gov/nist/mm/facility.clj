(ns dtype    (:use [tawny.owl]))
(ns vaem     (:use [tawny.owl]))
(ns qudt     (:use [tawny.owl]))
(ns modeling (:use [tawny.owl]))

(ns operations
  (:use [tawny.owl])
  (:require ;[dtype]
            ;[vaem]
            ;[qudt]
            [modeling]
            [tawny.reasoner]))

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
   org.semanticweb.owlapi.util.AutoIRIMapper
   [org.semanticweb.owlapi.search EntitySearcher])
  (:import java.net.URI
           java.io.ByteArrayInputStream)
  (:gen-class))

(def rrr "temporary, for debugging" (atom nil))
(def diag "temporary, for debugging" (atom nil))
(def onto-base "http://modelmeth.nist.gov/")

(def ontos "A map describing the ontologies used in this project"
;;;  {:dtype {:ns (find-ns 'dtype)
;;;           :iri "http://www.linkedmodel.org/schema/dtype"
;;;           :file "dtype.owl"}
;;;   :vaem  {:ns (find-ns 'vaem)
;;;           :iri "http://www.linkedmodel.org/schema/vaem"
;;;           :file "vaem.owl"}
;;;   :qudt  {:ns (find-ns 'qudt)
;;;           :iri "http://qudt.org/2.0/schema/qudt"
;;;           :file "qudt.ttl"}
   {:modeling   {:ns (find-ns 'modeling)
                 :iri (str onto-base "modeling")
                 :file "modeling.owl"}
    :operations {:ns (find-ns 'operations)
                 :iri (str onto-base "operations")
                 :file "operations.owl"}})

;;; This should be elsewhere. Probably a separate project shared by facility and ontolatex. 
;;; ================= Tawny stuff borrowed from ontolatex ====================

;;; Concepts:
;;; - Ontologies are associated with namespaces. See ref @tawny.owl/ontology-for-namespace.
;;; - You need to be in the namespace to do the operations. (tawny.owl/get-current-ontology)

(def tawny-types [:tawny.owl/class :tawny.owl/individual :tawny.owl/property
                  :tawny.owl/object-property :tawny.owl/data-property])

;;; POD Useless? 
(def manager (tawny.owl/owl-ontology-manager))

;;; POD I expected this to be http://modelmeth.nist.gov/modeling#clojureCodeNote
(def ^:const code-iri "Used to identify clojure notes from thing-mapped objects."
  (list :iri (str onto-base "modeling#clojureCode")))

(defn iri-frag
  [iri-str]
  (second (re-matches #"<.*\#(.+)>" iri-str)))

(defmacro with-onto [name & body]
  `(binding [*ns* (-> ontos ~name :ns)]
     ~@body))

(defn onto-get
  [st]
  (with-onto :operations
    (tawny.owl/entity-for-iri (IRI/create (str onto-base st)))))

;;; https://stackoverflow.com/questions/47394033/does-autoirimapper-cannot-read-ontologies-from-ttl-files
;;; "At present AutoIRIMapper only supports functional syntax, manchester syntax and RDF/XML."
;;; My experience is that they have to be RDF/XML and they have to be named .owl !!!
(defn add-mapper
  "Find onto file locally in resources directory."
  []
  (let [folder (clojure.java.io/file "resources")
        mapper (AutoIRIMapper. folder true)]
    (with-onto :modeling
      (-> (tawny.owl/owl-ontology-manager)
          .getIRIMappers
          (.add (vector mapper))))))

(defn my-intern-owl
  "Like tawny.owl/intern-owl, but interns in the current package, no hooks though.
   sym is a symbol."
  [sym entity]
  (let [var (intern *ns* sym entity)]
    (alter-meta! var #(merge % {:owl true}))
    var))

(defn intern-subclasses
  "Intern all the sublasses of the argument Entity, whereever they are."
  [ent]
  (loop [onames (map key ontos)]
    (when-let [oname (first onames)]
      (with-onto oname
        (let [o (tawny.owl/get-current-ontology)]
          (doall (map #(my-intern-owl (-> % .toString iri-frag symbol) %)
                      (tawny.owl/subclasses o ent)))))
      (when-not (empty? onames)
        (recur (rest onames))))))

(defn load-onto
  "Load an ontology. The argument is a keyword from the ontos map."
  [oname]
  (with-onto oname
    (add-mapper)
    (tawny.owl/remove-ontology-maybe
     (OWLOntologyID. (IRI/create (-> ontos oname :iri))))
    (tawny.owl/ontology-to-namespace
     (-> ontos oname :ns)
     (.loadOntologyFromOntologyDocument
      (tawny.owl/owl-ontology-manager)
      (IRI/create (clojure.java.io/resource (-> ontos oname :file)))))))

(defn load-ontos
  "Load all the project ontologies."
  []
  (doall (map load-onto (keys ontos)))
  (with-onto :operations
    ;(intern-subclasses (onto-get "modeling#Physical")) ; POD now that things are working...
    ;(intern-subclasses (onto-get "modeling#Abstract")) ; ...do I care to intern?
    (tawny.owl/owl-import 
     (tawny.owl/ontology-for-namespace (-> ontos :modeling :ns)))))
    
(defn onto-ref
  "Safely dereference a symbol to an ontology entry. Works after things are interned!"
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

(defn more-specific?
  "Compare owl classes"
  [x y]
  (cond ((tawny.owl/subclasses y) x) -1 
        (= x y) 0
        :else 1))

(defn more-general?
  "Compare owl classes"
  [x y]
  (cond ((tawny.owl/superclasses y) x) -1 
        (= x y) 0
        :else 1))

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

;;; POD Make this a table with "Production System Terms" and "Modeling Terms"
(defn- system-pg
  "Handle page for system tab."
  [request]
  (app-page-wrapper {:tab :system}
    (let [params (sparql/query mfg-kb '((?/x rdfs/subClassOf (onto-get "modeling#Physical"))))
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
         (assoc ?map :label sname) ; POD (or label)
         (assoc ?map :notes (simplify-tawny-annotations (:annotation ?map)))
         (assoc ?map :direct-subclasses (tawny.owl/direct-subclasses obj))
         (assoc ?map :superclasses (vec (sort more-specific? (tawny.owl/superclasses obj))))
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

