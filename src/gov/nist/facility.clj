(ns gov.nist.facility
  (:require [cheshire.core]
            [clojure.java.io :as io]
            [clojure.pprint :refer (cl-format pprint)]
            [clojure.string :as str]
            [compojure.route          :as route]
            [compojure.core     :refer (ANY GET defroutes)]
            [environ.core       :refer (env)]
            [hiccup.core :refer (html)]
            [mount.core         :refer [defstate]]
            [immutant.web             :as web]
            [immutant.web.middleware  :as immutant]
            [ring.util.response :refer (response #_redirect content-type)]
            [ring.util.codec    :refer (url-encode url-decode)]
;;;         [edu.ucdenver.ccp.kr.sparql :as sparql]
;;;         [edu.ucdenver.ccp.kr.jena.kb :as jena]
            [taoensso.timbre              :as log]
            [tawny.owl :as towl]
;;;         [tawny.query :as query]
            [tawny.render]
            [tawny.read :as rtowl]
            [tawny.lookup :as look])
  (:import ; I'll keep these here since if this grows, I might use a few. 
   (org.semanticweb.owlapi.model
    #_HasIRI
    #_OWLAxiom
    #_OWLEntity
    #_OWLObject
    #_#_OWLOntologyManager OWLOntology 
    #_OWLClassExpression OWLClass #_OWLAnnotation
    #_#_OWLDataProperty OWLObjectProperty
    #_OWLDataPropertyExpression
    #_#_OWLIndividual OWLDatatype
    #_OWLObjectPropertyExpression
    #_OWLNamedObject OWLOntologyID)
    org.semanticweb.owlapi.util.AutoIRIMapper
    #_[org.semanticweb.owlapi.search EntitySearcher])
  (:gen-class))

(def rrr "temporary, for debugging" (atom nil))
(def diag "temporary, for debugging" (atom nil))
(def onto-ns (create-ns 'onto))
  
;;; This should be elsewhere. Probably a separate project shared by facility and ontolatex. 
;;; ================= Tawny stuff borrowed from ontolatex ====================

;;; Concepts:
;;; - Ontologies are associated with namespaces. See ref @towl/ontology-for-namespace.
;;; - You need to be in the namespace to do the operations. (towl/get-current-ontology)

(def tawny-types [:towl/class :towl/individual :towl/property
                  :towl/object-property :towl/data-property])

;;; Useless? I get a problem with printing. So instead I use the ontos atom. 
(def manager (towl/owl-ontology-manager))

(def ontos "Map of all the ontologies read" (atom nil))

;;; POD I expected this to be http://modelmeth.nist.gov/modeling#clojureCodeNote
(def ^:const code-iri "Used to identify clojure notes from thing-mapped objects."
  (list :iri "http://modelmeth.nist.gov/modeling#clojureCode"))

(defn iri-frag
  [iri-str]
  (second (re-matches #"<.*\#(.+)>" iri-str)))

(defn onto-get
  [frag]
  (if-let [[onto-str term] (re-matches #"^(\w+)\#(\w+)$" frag)]
    (let [onto-map (get @ontos (keyword onto-str))
          onto (:onto onto-map)
          iri  (:iri onto-map)]
      (if-let [found (towl/entity-for-iri onto (towl/iri (str iri term)))]
        found
        (log/warn "Could not find term" frag)))
    (log/warn "Badly formed IRI fragment")))

;;; https://stackoverflow.com/questions/47394033/does-autoirimapper-cannot-read-ontologies-from-ttl-files
;;; "At present AutoIRIMapper only supports functional syntax, manchester syntax and RDF/XML."
(defn add-mapper
  "Find onto file locally in resources directory."
  []
  (let [folder (io/file "resources")
        mapper (AutoIRIMapper. folder true)]
    (-> (towl/owl-ontology-manager)
        .getIRIMappers
        (.add (vector mapper)))))

(defn my-intern-owl
  "Like towl/intern-owl, but interns in the current package, no hooks though.
   sym is a symbol."
  [sym entity]
  (let [var (intern *ns* sym entity)]
    (alter-meta! var #(merge % {:owl true}))
    var))

(defn intern-subclasses
  "Intern all the sublasses of the argument Entity, wherever they are."
  [ent]
  (loop [onames (map key ontos)]
    (when-let [_oname (first onames)]
      (let [o (towl/get-current-ontology)]
        (doall (map #(my-intern-owl (-> % .toString iri-frag symbol) %)
                    (towl/subclasses o ent)))))
    (when-not (empty? onames)
      (recur (rest onames)))))

(defn clear-ontos!
  "Remove all the ontologies"
  []
  (repeatedly
   5 ; POD 5 times!
   (fn []           
     (doall (map #(towl/remove-ontology-maybe (OWLOntologyID. (towl/iri %)))
                 (map :iri (-> ontos deref vals)))))))

(defn load-onto-files!
  "Load ontologies, returning a map of them."
  []
  {:dtype {:onto (rtowl/read :iri "http://www.linkedmodel.org/schema/dtype"
                             :namespace onto-ns
                             :location (clojure.java.io/file "resources/dtype.owl"))
           :iri "http://www.linkedmodel.org/schema/dtype"}
   :vaem  {:onto (rtowl/read :iri "http://www.linkedmodel.org/schema/vaem"
                             :namespace onto-ns
                             :location (clojure.java.io/file "resources/vaem.owl"))
           :iri "http://www.linkedmodel.org/schema/vaem"}
   :qudt  {:onto (rtowl/read :iri "http://qudt.org/2.0/schema/qudt"
                             :namespace onto-ns
                             :location (clojure.java.io/file "resources/SCHEMA_QUDT-v2.0.ttl"))
           :iri "http://qudt.org/2.0/schema/qudt"}
   :modeling {:onto (rtowl/read :iri "http://modelmeth.nist.gov/modeling"
                                :namespace onto-ns
                                :location (clojure.java.io/file "resources/modeling.ttl")) ; The .owl has a bug
              :iri "http://modelmeth.nist.gov/modeling"}
   ;; A few things interned (process
   :operations {:onto (rtowl/read :iri "http://modelmeth.nist.gov/operations"
                                  :namespace onto-ns
                                  :location (clojure.java.io/file "resources/operations.owl"))
                :iri "http://modelmeth.nist.gov/operations#"}})
 
(defn load-ontos
  "Load all the project ontologies."
  []
  (clear-ontos!)
  (reset! ontos (load-onto-files!))
  (intern-subclasses (onto-get "modeling#Physical")) ; POD now that things are working...  2021 these two were commented. 
  (intern-subclasses (onto-get "modeling#Abstract")) ; ...do I care to intern?
  (towl/owl-import 
   (towl/ontology-for-namespace onto-ns))
  true)
    
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
  (if (some #(= (towl/guess-type (towl/get-current-ontology) obj) %) tawny-types)
    (when-let [code (clojure-code obj)]
      (= :ignore (:priority (read-string code))))
    true))

(defn more-specific?
  "Compare owl classes"
  [x y]
  (cond ((towl/subclasses y) x) -1 
        (= x y) 0
        :else 1))

(defn more-general?
  "Compare owl classes"
  [x y]
  (cond ((towl/superclasses y) x) -1 
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
  (reset! rrr {:the-request request})
  (-> (response (with-out-str
                  (cl-format *out* "~%Call to echo returns:~%")
                  (pprint request)))
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
               `[:li {:class "active"} [:a {:href ~(str "/Facility/" (name k))} ~v]]
               `[:li                   [:a {:href ~(str "/Facility/" (name k))} ~v]]))
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
  (let [iri (.getIRI obj)
        name (->  iri .toString (subs 7) url-encode)
        sname (.getShortForm iri)]
    [:a {:href (str root "?name=" name)} sname]))

;;; POD Make this a table with "Production System Terms" and "Modeling Terms"
(defn- list-pg
  "Respond with a table containing system and modeling concepts each in a column."
  [request]
  (app-page-wrapper {:tab :system}
    (let [ops (->> (onto-get "operations#OperationsDomainConcept")
                   towl/subclasses
                   (sort #(let [name1 (-> %1 .getIRI .getShortForm)
                                name2 (-> %2 .getIRI .getShortForm)]
                            (compare name1 name2))))]
      (html
       [:h1 "Production System and Modeling Concepts"]
       `[:table {:width "60%"}
         [:tr [:th "System Concepts"] [:th "Modeling Concepts"]]
         ~@(map (fn [[sc mc]] [:tr [:td (owl-class-url sc)] [:td (owl-class-url mc)]])
                (map (fn [x] [x x]) ops))]))))

(defn- search-pg
  "Handle page for search tab."
  [_request]
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
  [_name]
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
         (assoc ?map :direct-subclasses (towl/direct-subclasses obj))
         (assoc ?map :superclasses (vec (sort more-specific? (towl/superclasses obj))))
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
  ;(GET "/" [] system-pg) ; POD investigte
  (GET "/facility/:tab" [tab] 
       (cond (= tab "list"  ) list-pg
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

;;;================== /facility/concepts?name=foo =====================================
;;; POD rewrite with reduce
(defn- table-vals
  "Returns vector of table rows (list of the row's elements) if TEXT (vector of strings) start a table."
  [text]
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

;;; (Re)start the server
(defn restart-web-server []
 (web/stop (-main :port 3034))
  (-main :port 3034))

(defstate facility
  :start
  (do (load-ontos)
      (restart-web-server)))
