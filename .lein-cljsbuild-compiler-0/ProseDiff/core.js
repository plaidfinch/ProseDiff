goog.provide('prosediff.core');
goog.require('cljs.core');
goog.require('clojure.string');
prosediff.core.word_chars = cljs.core.PersistentHashSet.fromArray(["A","a","B","b","C","c","D","d","E","e","F","f","'","G","g","H","h","I","i","J","j","K","k","L","l","-","M","m","N","n","O","o","0","P","p","1","Q","q","2","R","r","3","S","s","4","T","t","5","U","u","6","V","v","7","W","w","8","X","x","9","Y","y","Z","z"]);
/**
* @param {...*} var_args
*/
prosediff.core.split_to_words = (function() { 
var split_to_words__delegate = function (string,p__2851){
var map__2853 = p__2851;
var map__2853__$1 = ((cljs.core.seq_QMARK_.call(null,map__2853))?cljs.core.apply.call(null,cljs.core.hash_map,map__2853):map__2853);
var strip_spaces = cljs.core._lookup.call(null,map__2853__$1,"\uFDD0'strip-spaces",true);
return cljs.core.remove.call(null,(function (p1__2849_SHARP_){
var and__3822__auto__ = strip_spaces;
if(cljs.core.truth_(and__3822__auto__))
{return cljs.core._EQ_.call(null,p1__2849_SHARP_," ");
} else
{return and__3822__auto__;
}
}),cljs.core.map.call(null,cljs.core.partial.call(null,cljs.core.apply,cljs.core.str),cljs.core.partition_by.call(null,(function (p1__2850_SHARP_){
return cljs.core.complement.call(null,cljs.core.contains_QMARK_).call(null,prosediff.core.word_chars,p1__2850_SHARP_);
}),string)));
};
var split_to_words = function (string,var_args){
var p__2851 = null;
if (goog.isDef(var_args)) {
  p__2851 = cljs.core.array_seq(Array.prototype.slice.call(arguments, 1),0);
} 
return split_to_words__delegate.call(this, string, p__2851);
};
split_to_words.cljs$lang$maxFixedArity = 1;
split_to_words.cljs$lang$applyTo = (function (arglist__2854){
var string = cljs.core.first(arglist__2854);
var p__2851 = cljs.core.rest(arglist__2854);
return split_to_words__delegate(string, p__2851);
});
split_to_words.cljs$lang$arity$variadic = split_to_words__delegate;
return split_to_words;
})()
;
prosediff.core.indexed_word_list = (function indexed_word_list(word_list){
return cljs.core.map.call(null,cljs.core.vector,cljs.core.iterate.call(null,cljs.core.inc,0),word_list);
});
prosediff.core.pigeonhole = (function pigeonhole(L,key_func,val_func){
return cljs.core.apply.call(null,cljs.core.merge_with,cljs.core.comp.call(null,cljs.core.vec,cljs.core.flatten,cljs.core.vector),cljs.core.map.call(null,cljs.core.partial.call(null,cljs.core.apply,cljs.core.hash_map),cljs.core.map.call(null,(function (p1__2855_SHARP_){
return cljs.core.vector.call(null,key_func.call(null,p1__2855_SHARP_),cljs.core.vector.call(null,val_func.call(null,p1__2855_SHARP_)));
}),L)));
});
prosediff.core.map_over_values = (function map_over_values(f,m){
return cljs.core.into.call(null,cljs.core.empty.call(null,m),(function (){var iter__2498__auto__ = (function iter__2860(s__2861){
return (new cljs.core.LazySeq(null,false,(function (){
var s__2861__$1 = s__2861;
while(true){
if(cljs.core.seq.call(null,s__2861__$1))
{var vec__2863 = cljs.core.first.call(null,s__2861__$1);
var k = cljs.core.nth.call(null,vec__2863,0,null);
var v = cljs.core.nth.call(null,vec__2863,1,null);
return cljs.core.cons.call(null,cljs.core.PersistentVector.fromArray([k,f.call(null,v)], true),iter__2860.call(null,cljs.core.rest.call(null,s__2861__$1)));
} else
{return null;
}
break;
}
}),null));
});
return iter__2498__auto__.call(null,m);
})());
});
prosediff.core.word_indices_map = (function word_indices_map(word_list){
return prosediff.core.pigeonhole.call(null,prosediff.core.indexed_word_list.call(null,word_list),cljs.core.second,cljs.core.first);
});
prosediff.core.match_graph = (function match_graph(left_word_list,right_word_list){
var right_word_indices_map = prosediff.core.word_indices_map.call(null,right_word_list);
return cljs.core.map.call(null,(function (v){
return cljs.core.ObjMap.fromObject(["\uFDD0'word","\uFDD0'left-index","\uFDD0'right-indices"],{"\uFDD0'word":cljs.core.second.call(null,v),"\uFDD0'left-index":cljs.core.first.call(null,v),"\uFDD0'right-indices":cljs.core._lookup.call(null,right_word_indices_map,cljs.core.second.call(null,v),null)});
}),prosediff.core.indexed_word_list.call(null,left_word_list));
});
prosediff.core.first_edge = (function first_edge(graph_node){
return cljs.core.ObjMap.fromObject(["\uFDD0'word","\uFDD0'left-index","\uFDD0'right-index"],{"\uFDD0'word":(new cljs.core.Keyword("\uFDD0'word")).call(null,graph_node),"\uFDD0'left-index":(new cljs.core.Keyword("\uFDD0'left-index")).call(null,graph_node),"\uFDD0'right-index":cljs.core.first.call(null,(new cljs.core.Keyword("\uFDD0'right-indices")).call(null,graph_node))});
});
prosediff.core.rest_edges = (function rest_edges(graph_node){
if(cljs.core.empty_QMARK_.call(null,cljs.core.rest.call(null,(new cljs.core.Keyword("\uFDD0'right-indices")).call(null,graph_node))))
{return cljs.core.ObjMap.EMPTY;
} else
{return cljs.core.ObjMap.fromObject(["\uFDD0'word","\uFDD0'left-index","\uFDD0'right-indices"],{"\uFDD0'word":(new cljs.core.Keyword("\uFDD0'word")).call(null,graph_node),"\uFDD0'left-index":(new cljs.core.Keyword("\uFDD0'left-index")).call(null,graph_node),"\uFDD0'right-indices":cljs.core.rest.call(null,(new cljs.core.Keyword("\uFDD0'right-indices")).call(null,graph_node))});
}
});
prosediff.core.graph_node_edges = (function graph_node_edges(graph_node){
if(cljs.core.empty_QMARK_.call(null,(new cljs.core.Keyword("\uFDD0'right-indices")).call(null,graph_node)))
{return null;
} else
{return cljs.core.cons.call(null,prosediff.core.first_edge.call(null,graph_node),graph_node_edges.call(null,prosediff.core.rest_edges.call(null,graph_node)));
}
});
prosediff.core.sequence_to_range = (function sequence_to_range(s){
return cljs.core.ObjMap.fromObject(["\uFDD0'left-start","\uFDD0'right-start","\uFDD0'length","\uFDD0'words"],{"\uFDD0'left-start":(new cljs.core.Keyword("\uFDD0'left-index")).call(null,cljs.core.first.call(null,s)),"\uFDD0'right-start":(new cljs.core.Keyword("\uFDD0'right-index")).call(null,cljs.core.first.call(null,s)),"\uFDD0'length":(1 + ((new cljs.core.Keyword("\uFDD0'left-index")).call(null,cljs.core.last.call(null,s)) - (new cljs.core.Keyword("\uFDD0'left-index")).call(null,cljs.core.first.call(null,s)))),"\uFDD0'words":cljs.core.map.call(null,"\uFDD0'word",s)});
});
prosediff.core.match_with_single_sequence = (function match_with_single_sequence(sequence,graph_edge){
if((function (){var and__3822__auto__ = cljs.core._EQ_.call(null,(new cljs.core.Keyword("\uFDD0'left-index")).call(null,graph_edge),(1 + (new cljs.core.Keyword("\uFDD0'left-index")).call(null,cljs.core.first.call(null,sequence))));
if(and__3822__auto__)
{return cljs.core._EQ_.call(null,(new cljs.core.Keyword("\uFDD0'right-index")).call(null,graph_edge),(1 + (new cljs.core.Keyword("\uFDD0'right-index")).call(null,cljs.core.first.call(null,sequence))));
} else
{return and__3822__auto__;
}
})())
{return cljs.core.ObjMap.fromObject(["\uFDD0'match"],{"\uFDD0'match":cljs.core.cons.call(null,graph_edge,sequence)});
} else
{return cljs.core.ObjMap.fromObject(["\uFDD0'non-match"],{"\uFDD0'non-match":sequence});
}
});
prosediff.core.match_with_sequences = (function match_with_sequences(sequence_list,graph_edge){
var results = cljs.core.map.call(null,(function (p1__2864_SHARP_){
return prosediff.core.match_with_single_sequence.call(null,p1__2864_SHARP_,graph_edge);
}),sequence_list);
var matches = cljs.core.map.call(null,"\uFDD0'match",cljs.core.filter.call(null,"\uFDD0'match",results));
var non_matches = cljs.core.map.call(null,"\uFDD0'non-match",cljs.core.filter.call(null,"\uFDD0'non-match",results));
if(cljs.core.empty_QMARK_.call(null,matches))
{return cljs.core.ObjMap.fromObject(["\uFDD0'new","\uFDD0'non-matches"],{"\uFDD0'new":cljs.core.list.call(null,cljs.core.list.call(null,graph_edge)),"\uFDD0'non-matches":non_matches});
} else
{return cljs.core.ObjMap.fromObject(["\uFDD0'matches","\uFDD0'non-matches"],{"\uFDD0'matches":matches,"\uFDD0'non-matches":non_matches});
}
});
prosediff.core.extend_sequences_from_node = (function extend_sequences_from_node(sequence_list,graph_node){
var edges = prosediff.core.graph_node_edges.call(null,graph_node);
var results = cljs.core.ObjMap.fromObject(["\uFDD0'matches","\uFDD0'non-matches"],{"\uFDD0'matches":null,"\uFDD0'non-matches":sequence_list});
while(true){
if(cljs.core.empty_QMARK_.call(null,edges))
{return results;
} else
{var new_results = prosediff.core.match_with_sequences.call(null,(new cljs.core.Keyword("\uFDD0'non-matches")).call(null,results),cljs.core.first.call(null,edges));
{
var G__2866 = cljs.core.rest.call(null,edges);
var G__2867 = cljs.core.ObjMap.fromObject(["\uFDD0'matches","\uFDD0'non-matches"],{"\uFDD0'matches":cljs.core.concat.call(null,(new cljs.core.Keyword("\uFDD0'matches")).call(null,results),(new cljs.core.Keyword("\uFDD0'matches")).call(null,new_results),(new cljs.core.Keyword("\uFDD0'new")).call(null,new_results)),"\uFDD0'non-matches":(new cljs.core.Keyword("\uFDD0'non-matches")).call(null,new_results)});
edges = G__2866;
results = G__2867;
continue;
}
}
break;
}
});
prosediff.core.find_common_subsequences = (function find_common_subsequences(left_string,right_string){
return cljs.core.map.call(null,cljs.core.reverse,(function (){var graph = cljs.core.apply.call(null,prosediff.core.match_graph,cljs.core.map.call(null,cljs.core.comp.call(null,prosediff.core.split_to_words,(function (p1__2865_SHARP_){
return p1__2865_SHARP_.toLowerCase();
})),cljs.core.PersistentVector.fromArray([left_string,right_string], true)));
var live = null;
var complete = null;
while(true){
if(cljs.core.empty_QMARK_.call(null,graph))
{return cljs.core.concat.call(null,live,complete);
} else
{var new_sequences = prosediff.core.extend_sequences_from_node.call(null,live,cljs.core.first.call(null,graph));
{
var G__2868 = cljs.core.rest.call(null,graph);
var G__2869 = (new cljs.core.Keyword("\uFDD0'matches")).call(null,new_sequences);
var G__2870 = cljs.core.concat.call(null,complete,(new cljs.core.Keyword("\uFDD0'non-matches")).call(null,new_sequences));
graph = G__2868;
live = G__2869;
complete = G__2870;
continue;
}
}
break;
}
})());
});
prosediff.core.both_sides_range_subset_QMARK_ = (function both_sides_range_subset_QMARK_(A_range,B_range){
var and__3822__auto__ = ((new cljs.core.Keyword("\uFDD0'left-start")).call(null,A_range) >= (new cljs.core.Keyword("\uFDD0'left-start")).call(null,B_range));
if(and__3822__auto__)
{var and__3822__auto____$1 = ((new cljs.core.Keyword("\uFDD0'right-start")).call(null,A_range) >= (new cljs.core.Keyword("\uFDD0'right-start")).call(null,B_range));
if(and__3822__auto____$1)
{return (((new cljs.core.Keyword("\uFDD0'left-start")).call(null,A_range) + (new cljs.core.Keyword("\uFDD0'length")).call(null,A_range)) <= ((new cljs.core.Keyword("\uFDD0'left-start")).call(null,B_range) + (new cljs.core.Keyword("\uFDD0'length")).call(null,B_range)));
} else
{return and__3822__auto____$1;
}
} else
{return and__3822__auto__;
}
});
prosediff.core.remove_redundant_ranges = (function remove_redundant_ranges(ranges_list){
var r_map = prosediff.core.pigeonhole.call(null,ranges_list,"\uFDD0'left-start",cljs.core.identity);
return cljs.core.remove.call(null,cljs.core.empty_QMARK_,cljs.core.map.call(null,cljs.core.comp.call(null,(function (pair){
if(cljs.core.empty_QMARK_.call(null,cljs.core.filter.call(null,(function (p1__2871_SHARP_){
var and__3822__auto__ = prosediff.core.both_sides_range_subset_QMARK_.call(null,cljs.core.first.call(null,pair),p1__2871_SHARP_);
if(cljs.core.truth_(and__3822__auto__))
{return !(cljs.core._EQ_.call(null,p1__2871_SHARP_,cljs.core.first.call(null,pair)));
} else
{return and__3822__auto__;
}
}),cljs.core.second.call(null,pair))))
{return cljs.core.first.call(null,pair);
} else
{return null;
}
}),cljs.core.juxt.call(null,cljs.core.identity,cljs.core.comp.call(null,cljs.core.partial.call(null,cljs.core.apply,cljs.core.concat),cljs.core.partial.call(null,cljs.core.map,(function (p1__2872_SHARP_){
return cljs.core._lookup.call(null,r_map,p1__2872_SHARP_,null);
})),cljs.core.partial.call(null,cljs.core.apply,(function (p1__2873_SHARP_,p2__2874_SHARP_){
return cljs.core.range.call(null,(p1__2873_SHARP_ - p2__2874_SHARP_),((1 + p1__2873_SHARP_) + p2__2874_SHARP_));
})),cljs.core.juxt.call(null,"\uFDD0'left-start","\uFDD0'length")))),cljs.core.apply.call(null,cljs.core.concat,cljs.core.vals.call(null,r_map))));
});
prosediff.core.attach_IDs = (function attach_IDs(dict_list){
var id_list = cljs.core.range.call(null,cljs.core.count.call(null,dict_list));
var list_no_ids = dict_list;
var ids = id_list;
var list_with_ids = null;
while(true){
if(cljs.core.empty_QMARK_.call(null,list_no_ids))
{return list_with_ids;
} else
{{
var G__2875 = cljs.core.rest.call(null,list_no_ids);
var G__2876 = cljs.core.rest.call(null,ids);
var G__2877 = cljs.core.cons.call(null,cljs.core.conj.call(null,cljs.core.first.call(null,list_no_ids),cljs.core.ObjMap.fromObject(["\uFDD0'block-id"],{"\uFDD0'block-id":cljs.core.first.call(null,ids)})),list_with_ids);
list_no_ids = G__2875;
ids = G__2876;
list_with_ids = G__2877;
continue;
}
}
break;
}
});
prosediff.core.common_words_threshold = 2;
prosediff.core.remove_spurious = (function remove_spurious(left_string,right_string,ranges_list){
var vec__2880 = cljs.core.map.call(null,(function (string){
return cljs.core.comp.call(null,cljs.core.partial.call(null,prosediff.core.map_over_values,cljs.core.count),prosediff.core.word_indices_map,prosediff.core.split_to_words,(function (p1__2878_SHARP_){
return p1__2878_SHARP_.toLowerCase();
})).call(null,string);
}),cljs.core.PersistentVector.fromArray([left_string,right_string], true));
var left_counts = cljs.core.nth.call(null,vec__2880,0,null);
var right_counts = cljs.core.nth.call(null,vec__2880,1,null);
return cljs.core.filter.call(null,(function (r){
var or__3824__auto__ = ((function (){var or__3824__auto__ = cljs.core._lookup.call(null,left_counts,cljs.core.comp.call(null,cljs.core.str,"\uFDD0'words",r),null);
if(cljs.core.truth_(or__3824__auto__))
{return or__3824__auto__;
} else
{return 0;
}
})() <= prosediff.core.common_words_threshold);
if(or__3824__auto__)
{return or__3824__auto__;
} else
{return ((function (){var or__3824__auto____$1 = cljs.core._lookup.call(null,right_counts,cljs.core.comp.call(null,cljs.core.str,"\uFDD0'words",r),null);
if(cljs.core.truth_(or__3824__auto____$1))
{return or__3824__auto____$1;
} else
{return 0;
}
})() <= prosediff.core.common_words_threshold);
}
}),ranges_list);
});
prosediff.core.filtered_match_ranges = (function filtered_match_ranges(left_string,right_string){
return cljs.core.comp.call(null,prosediff.core.attach_IDs,prosediff.core.remove_redundant_ranges,cljs.core.partial.call(null,prosediff.core.remove_spurious,left_string,right_string),cljs.core.partial.call(null,cljs.core.map,prosediff.core.sequence_to_range),prosediff.core.find_common_subsequences).call(null,left_string,right_string);
});
prosediff.core.tagged_word_lists = (function tagged_word_lists(left_string,right_string){
prosediff.core.id_labeling_fold_func = (function id_labeling_fold_func(side,words_index_map,block_range){
var indices = cljs.core.range.call(null,side.call(null,block_range),(side.call(null,block_range) + (new cljs.core.Keyword("\uFDD0'length")).call(null,block_range)));
return cljs.core.reduce.call(null,(function (w_d,indx){
return cljs.core.update_in.call(null,w_d,cljs.core.PersistentVector.fromArray([indx,"\uFDD0'block-ids"], true),(function (L){
return cljs.core.cons.call(null,(new cljs.core.Keyword("\uFDD0'block-id")).call(null,block_range),L);
}));
}),words_index_map,indices);
});
var ranges_with_IDs = prosediff.core.filtered_match_ranges.call(null,left_string,right_string);
var vec__2882 = cljs.core.map.call(null,(function (string){
return cljs.core.into.call(null,cljs.core.sorted_map.call(null),cljs.core.map.call(null,(function (pair){
return cljs.core.PersistentVector.fromArray([cljs.core.first.call(null,pair),cljs.core.ObjMap.fromObject(["\uFDD0'word","\uFDD0'block-ids"],{"\uFDD0'word":cljs.core.second.call(null,pair),"\uFDD0'block-ids":null})], true);
}),prosediff.core.indexed_word_list.call(null,prosediff.core.split_to_words.call(null,string))));
}),cljs.core.PersistentVector.fromArray([left_string,right_string], true));
var left_words_index_map = cljs.core.nth.call(null,vec__2882,0,null);
var right_words_index_map = cljs.core.nth.call(null,vec__2882,1,null);
return cljs.core.map.call(null,(function (side,words_index_map){
return cljs.core.vals.call(null,cljs.core.reduce.call(null,cljs.core.partial.call(null,prosediff.core.id_labeling_fold_func,side),words_index_map,ranges_with_IDs));
}),cljs.core.PersistentVector.fromArray(["\uFDD0'left-start","\uFDD0'right-start"], true),cljs.core.PersistentVector.fromArray([left_words_index_map,right_words_index_map], true));
});
prosediff.core.reinsert_spaces = (function reinsert_spaces(t_w_list,string){
return cljs.core.map.call(null,(function (tagged,spacified){
return cljs.core.conj.call(null,tagged,spacified);
}),t_w_list,cljs.core.map.call(null,(function (w){
return cljs.core.ObjMap.fromObject(["\uFDD0'word"],{"\uFDD0'word":w});
}),(function (){var w_s = prosediff.core.split_to_words.call(null,string,"\uFDD0'strip-spaces",false);
var n_s = prosediff.core.split_to_words.call(null,string);
var result = null;
while(true){
if(cljs.core.empty_QMARK_.call(null,w_s))
{return cljs.core.reverse.call(null,result);
} else
{if(cljs.core._EQ_.call(null,cljs.core.first.call(null,w_s),cljs.core.first.call(null,n_s)))
{{
var G__2884 = cljs.core.rest.call(null,w_s);
var G__2885 = cljs.core.rest.call(null,n_s);
var G__2886 = cljs.core.cons.call(null,cljs.core.first.call(null,w_s),result);
w_s = G__2884;
n_s = G__2885;
result = G__2886;
continue;
}
} else
{{
var G__2887 = cljs.core.rest.call(null,w_s);
var G__2888 = n_s;
var G__2889 = cljs.core.cons.call(null,[cljs.core.str(cljs.core.first.call(null,result)),cljs.core.str(cljs.core.first.call(null,w_s))].join(''),cljs.core.rest.call(null,result));
w_s = G__2887;
n_s = G__2888;
result = G__2889;
continue;
}
}
}
break;
}
})()));
});
prosediff.core.tagged_word_to_HTML = (function tagged_word_to_HTML(element,move_class,nil_class,word){
return [cljs.core.str("<"),cljs.core.str(element),cljs.core.str(" class=\""),cljs.core.str(cljs.core.apply.call(null,cljs.core.str,cljs.core.cons.call(null,((cljs.core.empty_QMARK_.call(null,(new cljs.core.Keyword("\uFDD0'block-ids")).call(null,word)))?nil_class:move_class),cljs.core.map.call(null,(function (p1__2883_SHARP_){
return [cljs.core.str(" pd-block-"),cljs.core.str(p1__2883_SHARP_)].join('');
}),(new cljs.core.Keyword("\uFDD0'block-ids")).call(null,word))))),cljs.core.str("\">"),cljs.core.str(clojure.string.replace.call(null,(new cljs.core.Keyword("\uFDD0'word")).call(null,word),"\n","<br/>")),cljs.core.str("</"),cljs.core.str(element),cljs.core.str(">")].join('');
});
prosediff.core.generate_HTML = (function generate_HTML(element,move_class,nil_class,string,tagged_words){
return cljs.core.apply.call(null,cljs.core.str,cljs.core.map.call(null,cljs.core.partial.call(null,prosediff.core.tagged_word_to_HTML,element,move_class,nil_class),prosediff.core.reinsert_spaces.call(null,tagged_words,string)));
});
/**
* Recursively transforms ClojureScript maps into Javascript objects,
* other ClojureScript colls into JavaScript arrays, and ClojureScript
* keywords into JavaScript strings.
*/
prosediff.core.clj_js = (function clj_js(x){
if(cljs.core.string_QMARK_.call(null,x))
{return x;
} else
{if(cljs.core.keyword_QMARK_.call(null,x))
{return cljs.core.name.call(null,x);
} else
{if(cljs.core.map_QMARK_.call(null,x))
{return cljs.core.reduce.call(null,(function (m,p__2892){
var vec__2893 = p__2892;
var k = cljs.core.nth.call(null,vec__2893,0,null);
var v = cljs.core.nth.call(null,vec__2893,1,null);
return cljs.core.assoc.call(null,m,clj_js.call(null,k),clj_js.call(null,v));
}),cljs.core.ObjMap.EMPTY,x).strobj;
} else
{if(cljs.core.coll_QMARK_.call(null,x))
{return cljs.core.apply.call(null,cljs.core.array,cljs.core.map.call(null,clj_js,x));
} else
{if("\uFDD0'else")
{return x;
} else
{return null;
}
}
}
}
}
});
prosediff.core.process = (function process(left_string,right_string){
return prosediff.core.clj_js.call(null,cljs.core.map.call(null,cljs.core.partial.call(null,prosediff.core.generate_HTML,"span","pd-move"),cljs.core.PersistentVector.fromArray(["pd-delete","pd-insert"], true),cljs.core.PersistentVector.fromArray([left_string,right_string], true),prosediff.core.tagged_word_lists.call(null,left_string,right_string)));
});
goog.exportSymbol('prosediff.core.process', prosediff.core.process);
