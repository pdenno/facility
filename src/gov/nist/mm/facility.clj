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
            
            [edu.ucdenver.ccp.kr.kb :as kb]
            [edu.ucdenver.ccp.kr.rdf :as rdf]
            [edu.ucdenver.ccp.kr.sparql :as sparql]
            [edu.ucdenver.ccp.kr.jena.kb :as jena]
            
            [tawny.owl :as owl]
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

(def rrr (atom nil))


;;; ================= Tawny stuff borrowed from ontolatex ====================
(def tawny-types [:tawny.owl/class :tawny.owl/individual :tawny.owl/property
                  :tawny.owl/object-property :tawny.owl/data-property])

(def ontologies "A list of the ontologies used in this project"
  ["http://modelmeth.nist.gov/ontologies/pizza/pizza.owl"
   "http://www.linkedmodel.org/schema/dtype"
   "http://www.linkedmodel.org/schema/vaem"
   "http://qudt.org/2.0/schema/qudt"
   "http://modelmeth.nist.gov/modeling"
   "http://modelmeth.nist.gov/operations"])

;;; POD I expected this to be http://modelmeth.nist.gov/modeling#clojureCodeNote
(def ^:const code-iri "Used to identify clojure notes from thing-mapped objects."
  (list :iri "http://modelmeth.nist.gov/modeling#clojureCode"))

(defn read-base-ontos!
  []
  (rowl/read :iri "http://www.linkedmodel.org/schema/dtype"
             :namespace (create-ns 'onto) ; was onto.dtype
             :location (clojure.java.io/file "resources/dtype.owl"))
  (rowl/read :iri "http://www.linkedmodel.org/schema/vaem"
             :namespace (create-ns 'onto) ; was onto.vaem
             :location (clojure.java.io/file "resources/vaem.owl"))
  (rowl/read :iri "http://qudt.org/2.0/schema/qudt"
             :namespace (create-ns 'onto) ; was onto.qudt
             :location (clojure.java.io/file "resources/SCHEMA_QUDT-v2.0.ttl")))

(defn clear-ontos!
  "Remove all the ontologies"
  []
  (repeatedly
   5
   (fn []           
     (doall (map #(owl/remove-ontology-maybe (OWLOntologyID. (owl/iri %))) ontologies)))))

(defn load-kb
  "Return a KB object with input loaded."
  [& files]
  (let [kb (kb/kb :jena-mem)]
    (dorun (map #(rdf/load-rdf kb (ByteArrayInputStream. (.getBytes (slurp %))) :turtle) files))
    (rdf/synch-ns-mappings kb)))

(def mfg-kb (load-kb (clojure.java.io/resource "operations.ttl")
                     (clojure.java.io/resource "toplevel-taxonomies.ttl")))



(defn load-kb []
  (clear-ontos!)
  (read-base-ontos!)
  (rowl/read :iri (str "http://modelmeth.nist.gov/" uri-base)
             :namespace (create-ns (:onto-namespace @+params+)) 
             :location (clojure.java.io/file onto-file))



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
  (app-page-wrapper {:tab :system}
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

#_(defn thing-map
  "Return a map of information about the class. See also query/into-map-with"
  ([obj] (thing-map obj {})) ; Used by ignore? 
  ([obj property-map]
   (when (instance? OWLClass obj)
     (let [sname (short-name obj)]
       (as-> (apply hash-map (tawny.render/as-form obj :keyword true)) ?map
         (assoc ?map :short-name sname) ; POD (or label)
         (assoc ?map :var (intern (:onto-namespace @+params+) (symbol sname)))
         (assoc ?map :notes (simplify-tawny-annotations (:annotation ?map)))
         (assoc ?map :subclass-of (doall (map short-name ; POD there are other ways. See notes 2017-07-22. 
                                              (filter #(instance? OWLClass %)
                                                      (owl/direct-superclasses obj)))))
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

