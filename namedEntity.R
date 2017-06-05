library(coreNLP)
initCoreNLP(mem = "4g", annotators = c("tokenize", "ssplit", "pos","lemma","ner","parse","dcoref"))
library(xlsx)
library(tm)

library(openNLP)

library(data.tree)

library(igraph)
library("RSelenium")
library(httr)
checkForServer()
startServer()
mybrowser <-  remoteDriver()
mybrowser$open()
mybrowser$navigate("http://ucrel.lancs.ac.uk/usas/tagger.html")

sent_token_annotator <- Maxent_Sent_Token_Annotator() 
word_token_annotator <- Maxent_Word_Token_Annotator() 
parse_annotator <- Parse_Annotator() 
pos_tag_annotator <- Maxent_POS_Tag_Annotator() 


#dictionaries
abbreviations<-read.xlsx("/Users/Tome/Desktop/NameEntity/Dictionaries/Abbreviations.xlsx",sheetName="Sheet1")
abbreviations<-as.character(abbreviations$x)
metric_kitchen<-read.xlsx("/Users/Tome/Desktop/NameEntity/Dictionaries/metric_kitchen.xlsx",sheetName="Sheet1",header=FALSE)
metric_kitchen<-tolower(as.character(metric_kitchen$X1))



test_data<-read.xlsx("/Users/Tome/Desktop/test_set.xlsx",sheetName="Sheet1")
paragraphs<-as.character(test_data$Text)


# function for spliting text on sentences

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  
  # Convert text to class String from package NLP
  text <- as.String(text)
  
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  
  # Extract sentences
  sentences <- text[sentence.boundaries]
  
  # return sentences
  return(sentences)
}


# link to the Food dictionary (use the USAS Semantic English tagger (annotator)), return food vector


Food_checking<-function(s,df){
	foods_check<-rep(0,nrow(df))

	
	recommendations<-s
	mybrowser$navigate("http://ucrel.lancs.ac.uk/usas/tagger.html")
	webElem<-mybrowser$findElement(using = 'name', value="text" )
	webElem$clearElement()
	webElem$sendKeysToElement(list(recommendations))
	submitElem<-mybrowser$findElement( using='xpath',"//input[@value='Tag text now']" )
	submitElem$clickElement()
	result<-mybrowser$findElements(using='xpath',"//pre")
	results <- unlist(lapply(result, function(x){x$getElementText()}))
	foods<-grep("_F(1|2|3|4)|_L(2|3)|F(1|2|3|4)|L(2|3)",strsplit(results," ")[[1]], value=TRUE)
	# indices_food<-grep("_F(1|2|3|4)|_L(2|3)",strsplit(results," ")[[1]], value=FALSE)
	
	if(length(foods)!=0){
	foods_temp<-strsplit(foods,"_")
	
	foods_words<-c()
	
	for(i in 1:length(foods_temp)){
		foods_words[i]<-gsub("\\n","",strsplit(foods,"_")[[i]])
	}
	
	for(i in 1:length(foods_words)){
		foods_check[which(df$words==foods_words[i])]<-1
	}
	}

	return(foods_check)
	
	
	
	}

	
	
	
	





# link to the Chemical named entity recogniton (using becas and becas API)
# input data frame that is result from the POStagging function, chemics is a list with nutrient componenets for each recommendation that is in the evaluation set
# needs on input chemics[[i]]
Nutrient_checking<-function(df,s){
  nutrients_USAS_tagger<-c()
  nutrient<-rep(0,nrow(df))
  words<-df$words
   # result_json<-POST("https://www.ncbi.nlm.nih.gov/CBBresearch/Lu/Demo/RESTful/tmTool.cgi/tmChem/Submit/",body=s)
  # result<-GET(result_json$url)
   # while(result$status!=200){
  	  # result<-GET(result_json$url)
   # }
   # text_content<- a<-content(result, "text", encoding = "ISO-8859-1")
   # elements<-strsplit(text_content,"\\t")
   # indices<-grep("Chemical",elements[[1]])
   # nutrients_tmChem<-c()
  # if(length(indices)!=0){
   # indices<-indices-1
   # nutrients_tmChem<-elements[[1]][indices]
  
  
  
   # }
  
  recommendations<-s
	mybrowser$navigate("http://ucrel.lancs.ac.uk/usas/tagger.html")
	webElem<-mybrowser$findElement(using = 'name', value="text" )
	webElem$clearElement()
	webElem$sendKeysToElement(list(recommendations))
	submitElem<-mybrowser$findElement( using='xpath',"//input[@value='Tag text now']" )
	submitElem$clickElement()
	result<-mybrowser$findElements(using='xpath',"//pre")
	results <- unlist(lapply(result, function(x){x$getElementText()}))
	nutrients<-grep("_O(1[\\.(1|2|3)]?)|O(1[\\.(1|2|3)]?)",strsplit(results," ")[[1]], value=TRUE)
	# indices_food<-grep("_F(1|2|3|4)|_L(2|3)",strsplit(results," ")[[1]], value=FALSE)
	
	if(length(nutrients)!=0){
	nutrients_temp<-strsplit(nutrients,"_")
	
	nutrients_words<-c()
	
	for(i in 1:length(nutrients_temp)){
		nutrients_words[i]<-gsub("\\n","",strsplit(nutrients,"_")[[i]])
	}
	}
	
	
  
 postfield<-list(groups=list(CHED=TRUE),text=s)
result_json<-POST("http://bioinformatics.ua.pt/becas/api/text/annotate?email=tomeeftimov@gmail.com&tool=curl-samples-test",add_headers(.headers = c('Content-Type'="application/json")),body=postfield,encode="json")
	nutrients_becas<-c()
	if(length(content(result_json)$entities)!=0){
	for(j in 1:length(content(result_json)$entities)){
		nutrients_becas[j]<-strsplit(content(result_json)$entities[[j]],"\\|")[[1]][1]
	
	
	}
	}
	
	
	 postfield<-list(text=s)
result_json<-POST("http://bioinformatics.ua.pt/becas-chemicals/api/text/annotate?email=tomeeftimov@gmail.com&tool=curl-samples-test",add_headers(.headers = c('Content-Type'="application/json")),body=postfield,encode="json")
	nutrients_becas_chemicals<-c()
	if(length(content(result_json)$entities)!=0){
	for(j in 1:length(content(result_json)$entities)){
		nutrients_becas_chemicals[j]<-strsplit(content(result_json)$entities[[j]],"\\|")[[1]][1]
	
	
	
	}
	}

	if(length(nutrients_becas)!=0){
		nutrients_becas<-unlist(strsplit(nutrients_becas," "))
	}
	
	if(length(nutrients_becas_chemicals)!=0){
		nutrients_becas_chemicals<-unlist(strsplit(nutrients_becas_chemicals," "))
	}
	
	if(length(nutrients_becas)!=0 || length(nutrients_becas_chemicals)!=0 || length(nutrients_words)!=0){
	nutrients<-unique(union(union(nutrients_becas_chemicals,nutrients_becas),nutrients_words))
	if(length(nutrients)!=0){
 		for(i in 1:length(nutrients)){
  	 		nutrient[which(words==nutrients[i])]<-1
   		}
	}
}

  return(nutrient)
}


#check for units
Unit_checking<-function(df_res,abbreviations, metric_kitchen){
  unit<-rep(0,nrow(df_res))
  words<-df_res$words
  tags<-df_res$tags
  lemmas<-df_res$lemmas
  for(i in 1:length(words)){
    if(length(grep("NN[A-Z]*",tags[i]))==1){
     
      t<-lemmas[i]
      df<-data.frame(t)
      word<-as.character(df$t)
      
    }
    else{
      word<-words[i]
    }
    
    
    ifelse((length(which(abbreviations==word))!=0 || length(which(metric_kitchen==word))!=0) && length(grep("(NN[A-Z]*|CD|JJ)",tags[i]))==1 && words[i]!="a" && words[i]!="b" && words[i]!="c" && words[i]!="d" && words[i]!="us",unit[i]<-1,unit[i]<-0)
    
  }
  
  return(unit)
  
}


#POS tagging - return words together with the punctuations ( whitout .) and their POS taggs
POStagging<-function(s){
  annotObj <- annotateString(s)
  tokens<-getToken(annotObj)
  
  words<-tokens$token
  tags<-tokens$POS
  lemmas<-tokens$lemma
 
  df<-data.frame(words,tags,lemmas)
  return(df)
  
  
}

Default_chunking<-function(s){
	a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
	a3 <- annotate(s, pos_tag_annotator, a2)
	a3w <- subset(a3, type == "word") 
	tags <- sapply(a3w$features, `[[`, "POS")
	
	a4<-annotate(s, Maxent_Chunk_Annotator(), a3)
	a4w <- subset(a4, type == "word") 
	chunks<-sapply(a4w$features, `[[`, "chunk_tag")
	
	beginnings1<- grep("^B",chunks)
	beginnings2<-grep("^O",chunks)
	beginnings<-sort(c(beginnings1,beginnings2))
	
	
	return(list(chunks=chunks,beginnings=beginnings))
	
}



# Generate the chunk matrix with dimension chunks x words
# Inputs: data frame from POStagging function and data frame from Default chunking function
Chunk_matrix<-function(df_default_chunking,df_pos){
	
	words<-df_pos$words
	tags<-df_pos$tags
	
	chunks<-df_default_chunking$chunks
	beginnings<-df_default_chunking$beginnings
	
	A<-matrix(0,length(words),length(beginnings))
	
	for(j in 1:(length(beginnings)-1)){
	
		for(i in 1:length(words)){
		
			if(i<beginnings[j+1] && i>=beginnings[j]){
				A[i,j]<-1
			}

			
		}
	}
	
	for( i in beginnings[length(beginnings)]:length(chunks)){
		A[i,length(beginnings)]<-1
	}
		
		
	return(A)
	
}

# Genrate the Entities matrix with dimension words x labels (entities)
Entities_matrix<-function(food, unit, nutrient){
	
	
	and<-c()
	for(i in 1:length(food)){
	
		and[i]<-food[i]&&nutrient[i]
	}

	for(i in 1:length(food)){
		if(and[i]==TRUE){
			nutrient[i]<-0
		}
	
	}

	and<-c()
	for(i in 1:length(unit)){
	
		and[i]<-unit[i]&&nutrient[i]
	}

	for(i in 1:length(unit)){
		if(and[i]==TRUE){
			unit[i]<-0
		}
	
	}
	
	
	and<-c()
	for(i in 1:length(food)){
	
		and[i]<-unit[i]&&food[i]
	}

	for(i in 1:length(unit)){
		if(and[i]==TRUE){
			food[i]<-0
		}
	
	}



# Create matrix C  with dimension words x lables (food,nutrient,unit)

C<-as.matrix(data.frame(food,nutrient,unit))

return(C)
}


# Calculate the Candidates matrix
# Inputs: A is the result from the Chunk_matrix function, C is the result from the Entities_matrix function
Candidates_matrix<-function(A,C){
	
	B<-t(A)%*%C

	B[B>=1]<-1
	
	return(B)
}

# First additional chunking, (NP,PP,NP) ...., return the result from the first additional chunking
# Inputs: data frame that is the result form the Default chunking function and the Candidates matrix from the Candidates_matrix function
First_chunking<-function(df_default_chunking, Candidates){
	
	chunks<-df_default_chunking$chunks
	chunks1<-df_default_chunking$chunks
	B<-Candidates
	beginnings<-df_default_chunking$beginnings
	
	
	chunks_unit<-B[,3]
	chunks_nutrient<-B[,2]
	chunks_food<-B[,1]
	
	for(i in 1:length(beginnings)){
		if(length(grep("[BI]-VP",chunks1[beginnings[i]]))==1 && length(grep("[B]-VP",chunks1[beginnings[i+1]]))==1){
			chunks1[beginnings[i+1]]<-"I-VP"	
		}
	
	}
	
	for(i in 1:length(beginnings)){

		if(length(grep("[BI]-NP",chunks1[beginnings[i]]))==1 && length(grep("(B-PP|B-ADJP)",chunks1[beginnings[i+1]]))==1 && length(grep("B-NP",chunks1[beginnings[i+2]]))==1 && ((chunks_unit[i]==0 || chunks_nutrient[i+2]==0) && (chunks_nutrient[i]==0 || chunks_unit[i+2]==0) && (chunks_unit[i]==0 || chunks_food[i+2]==0) && (chunks_food[i]==0 || chunks_unit[i+2]==0) && (chunks_food[i]==0 || chunks_nutrient[i+2]==0) && (chunks_nutrient[i]==0 || chunks_food[i+2]==0)) ){
			chunks1[beginnings[i+1]]<-"I-NP"
			chunks1[beginnings[i+2]]<-"I-NP"
			if(chunks_unit[i]==1){
				chunks_unit[i+1]<-1
				chunks_unit[i+2]<-1
			
			}
			if(chunks_food[i]==1){
				chunks_food[i+1]<-1
				chunks_food[i+2]<-1
			
			}
			if(chunks_nutrient[i]==1){
				chunks_nutrient[i+1]<-1
				chunks_nutrient[i+2]<-1
			
			}
		}
	
	}
	
	return(chunks1)

}


#Second additional chunking, (NP,VP,NP, where the first NP is WP-POS tag)
# Inputs: chunks that are results by intorducing the first additional chunking, data frame with tokens and pos tags, fodd, nutreint, and unit
Second_chunking<-function(chunks1,df_pos,food,unit,nutrient){
	beginnings_chunks11<- grep("^B",chunks1)
	beginnings_chunks12<-grep("^O",chunks1)
	beginnings1<-sort(c(beginnings_chunks11,beginnings_chunks12))
	chunks2<-chunks1
	tags<-df_pos$tags
	words<-df_pos$words
	
	for(i in 1:length(beginnings1)){
	
	if(length(grep("[B]-NP",chunks2[beginnings1[i]]))==1 && length(grep("B-VP",chunks2[beginnings1[i+1]]))==1 && length(grep("[B]-NP",chunks2[beginnings1[i+2]]))==1 && length(grep("WP",tags[beginnings1[i]]))==1){
		
			chunks2[beginnings1[i+2]]<-"I-NP"
			index<-seq(beginnings1[i+1],beginnings1[i+2]-1,1)
			for(j in index){
				chunks2[j]<-"I-NP"
			}
	
		
		}
	}
	beginnings_chunks21<- grep("^B",chunks2)
	beginnings_chunks22<-grep("^O",chunks2)
	beginnings2<-sort(c(beginnings_chunks21,beginnings_chunks22))
	
	AA<-matrix(0,length(words),length(beginnings2))



	for(j in 1:(length(beginnings2)-1)){
	
		for(i in 1:length(words)){
		
			if(i<beginnings2[j+1] && i>=beginnings2[j]){
				AA[i,j]<-1
			}

			
		}
	}
	
	for( i in beginnings2[length(beginnings2)]:length(chunks2)){
		AA[i,length(beginnings2)]<-1
	}
	
	C<-as.matrix(data.frame(food,nutrient,unit))

	BB<-t(AA)%*%C
	
	return(list(chunks2=chunks2,B=BB))
	
	
	
}

#Third additional chunnking, NP NP (now only for units, needs  to be done for all others ???)
#Inputs: result from the second additional chunking, data frame with tokens and pos tags

Third_chunking<-function(output, df_pos, food, unit, nutrient){
	
	words<-df_pos$words
	tags<-df_pos$tags
	
	chunks2<-output$chunks2
	B<-output$B
	
	beginnings_chunks21<- grep("^B",chunks2)
	beginnings_chunks22<-grep("^O",chunks2)
	beginnings2<-sort(c(beginnings_chunks21,beginnings_chunks22))
	
	chunks2_unit<-B[,3]
	chunks2_food<-B[,1]
	chunks2_nutrient<-B[,2]
	chunks3<-chunks2

	for(i in 1:length(beginnings2)){
	
		if(length(grep("[B]-NP",chunks3[beginnings2[i]]))==1 && length(grep("B-NP",chunks3[beginnings2[i+1]]))==1 && (chunks2_unit[i]==1 || chunks2_unit[i+1]==1)){
		
			chunks3[beginnings2[i+1]]<-"I-NP"
		
		
	
		
		}

	}
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))
	
	
	AAA<-matrix(0,length(words),length(beginnings3))



	for(j in 1:(length(beginnings3)-1)){
	
		for(i in 1:length(words)){
		
			if(i<beginnings3[j+1] && i>=beginnings3[j]){
				AAA[i,j]<-1
			}

			
		}
	}
	for( i in beginnings3[length(beginnings3)]:length(chunks3)){
		AAA[i,length(beginnings3)]<-1
	}
	
	C<-as.matrix(data.frame(food,nutrient,unit))
	BBB<-t(AAA)%*%C

	BBB[BBB>=1]<-1

	unit_final<-BBB[,3]
	food_final<-BBB[,1]
	nutrient_final<-BBB[,2]

	and<-c()
	for(i in 1:nrow(BBB)){
	
		and[i]<-food_final[i]&&nutrient_final[i]
	}

	for(i in 1:length(food_final)){
		if(and[i]==TRUE){
			nutrient_final[i]<-0
		}
	
	}

	and<-c()
	for(i in 1:nrow(BBB)){
	
		and[i]<-unit_final[i]&&nutrient_final[i]
	}

	for(i in 1:length(unit_final)){
		if(and[i]==TRUE){
			unit_final[i]<-0
		}
	
	}

	and<-c()
	for(i in 1:nrow(BBB)){
	
		and[i]<-unit_final[i]&&food_final[i]
	}

	for(i in 1:nrow(BBB)){
		if(and[i]==TRUE){
			food_final[i]<-0
		}
	
	}
	BBB[,1]<-food_final
	BBB[,2]<-nutrient_final
	BBB[,3]<-unit_final

	
	return(list(chunks3=chunks3,B=BBB))

}
# Obtain candidate phrases
#Inputs: result from the third chunking, data frame with tokens and pos tags
Phrases<-function(output,df_pos)
{
	chunks3<-output$chunks3
	
	B<-output$B
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("^O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))
	
	words<-df_pos$words
	tags<-df_pos$tags
	
	phrases<-c()
	for( i in 1:(length(beginnings3)-1)){
		indices<-seq(beginnings3[i],beginnings3[i+1]-1,1)
		phrases[i]<-paste(words[indices],sep="",collapse=" ")
	}
	indices<-seq(beginnings3[i+1],length(chunks3),1)
	phrases[i+1]<-paste(words[indices],sep="",collapse=" ")


	return(phrases)

	
}

#Inputs> result from the thrd additional chunking, and phrases
Graph_recommendation<-function(output,phrases){
	
	chunks3<-output$chunks3
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("^O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))
	
	graph<-matrix(0,length(beginnings3),length(beginnings3))

	for(i in 1:nrow(graph)){
		for(j in 1:ncol(graph)){
			if(j==i+1){
				graph[i,j]<-1
			}
			if(j==i-1){
				graph[i,j]<-1
			}
		}
	}

	#create graph
	g <- graph.adjacency(graph, mode = "undirected")
	g <- simplify(g)
	V(g)$label <- phrases
	V(g)$degree <- degree(g)
	
	return(g)


	
}


Plot_graph<-function(g){
	
set.seed(3952)
layout1 <- layout.fruchterman.reingold(g)
plot(g, layout=layout1)


	
}


#split on ADVP, return the sentences 

#if sentences is null, there is not advp phrases
Split_ADVP<-function(df_pos, df_default_chunking,s){
	chunks<-df_default_chunking$chunks
	words<-as.character(df_pos$words)
	tags<-as.character(df_pos$tags)
	s_temp<-s
	
	sbar<-grep("B-ADVP|B-SBAR|B-CONJP",chunks)
	
	# advp<-c()
	# advp_temp<-grep("B-ADVP|I-ADVP",chunks)
	# counter<-1
	# for(i in 1:length(advp_temp)){
		# if(tags[advp_temp[i]]=="IN"){
			# advp[counter]<-advp_temp[i]
			# counter<-counter+1
		# }
	# }
	
	cc<-c()
	for(i in 1:(length(chunks)-1)){
		if(chunks[i]=="O" && chunks[i+1]=="O"){
			cc<-c(cc,i)
		}
	}
	
	advp<-c(sbar,cc)
	advp<-sort(advp)
	sentences<-c()
	count<-1
	if(length(advp)!=0){
		
				sentences[count]<-paste(df_pos$words[1:(advp[1]-1)],sep=" ",collapse=" ")
				sentences[count]<-paste(sentences[count],".",sep="",collapse="")
				count<-count+1
		
		if(length(advp)>1){
		for( i in 1:(length(advp)-1)){
						
	
			
			
				sentences[count]<-paste(df_pos$words[(advp[i]+1):(advp[i+1]-1)],sep=" ",collapse=" ")
				sentences[count]<-paste(sentences[count],".",sep="",collapse="")

				count<-count+1
				
			}
			}
			
			sentences[count]<-paste(df_pos$words[(advp[length(advp)]+1):length(df_pos$words)],sep=" ",collapse=" ")
			

				count<-count+1

			
				

	

	
	sentences<-gsub("^,","",sentences)
	sentences<-gsub("^ ","",sentences)
	sentences<-gsub("^,","",sentences)
	sentences<-gsub("^ ","",sentences)
	sentences<-gsub(" $","",sentences)
	sentences<-gsub(",$","",sentences)
	sentences<-gsub(" $","",sentences)
	sentences<-gsub(",$","",sentences)
	sentences<-gsub("(^[[:alpha:]])", "\\U\\1", sentences, perl=TRUE)


	for(i in 1:length(sentences)){
		if(length(grep("\\.$",sentences[i]))==0){
			sentences[i]<-paste(sentences[i],".",sep="",collapse="")
		}
	
	}
	}
	
	
	

	
	return(sentences)
	
}


#Select the Action entity
#Inputs: recommendation, phrases, and the output of the third additional chunking

Select_Action_Subjects<-function(s,phrases,output){
	
	B<-output$B
	chunks3<-output$chunks3
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("^O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))
	
	stopifnot(require(NLP) && require(igraph))

 	a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
 	p <- parse_annotator(s, a2)
  	## Extract the formatted parse trees. 
  	ptexts <- sapply(p$features, `[[`, "parse") 
 
  	## Read into NLP Tree objects. 
  	ptrees <- lapply(ptexts, Tree_parse) 
  
  	ptext<-ptexts
  	
  	ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
    words <- regmatches(ptext, ms)[[1]]                                   # just words
    regmatches(ptext, ms) <- list(paste0(words, seq.int(length(words))))  # add id to words

    ## Going to construct an edgelist and pass that to igraph
    ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
    edgelist <- matrix('', nrow=length(words)-2, ncol=2)

    ## Function to fill in edgelist in place
    edgemaker <- (function() {
        i <- 0                                       # row counter
        g <- function(node) {                        # the recursive function
            if (inherits(node, "Tree")) {            # only recurse subtrees
                if ((val <- node$value) != 'TOP1') { # skip 'TOP' node (added '1' above)
                    for (child in node$children) {
                        childval <- if(inherits(child, "Tree")) child$value else child
                        i <<- i+1
                        edgelist[i,1:2] <<- c(val, childval)
                    }
                }
                invisible(lapply(node$children, g))
            }
        }
    })()

    ## Create the edgelist from the parse tree
    edgemaker(Tree_parse(ptext))

    ## Make the graph, add options for coloring leaves separately
    g <- graph_from_edgelist(edgelist)
	tree <- FromDataFrameNetwork(as.data.frame(edgelist))
	SetNodeStyle(tree, style = "filled,rounded", shape = "box", fillcolor = "GreenYellow")
	
	Adj<-get.adjacency(g)
  	d<-as.data.frame(as.matrix(Adj))
	cols<-colnames(d)
	rows<-rownames(d)
	
	action_candidates<-rows[grep("VP|VB|MD",rows)]
	index_candidates<-grep("VP|VB|MD",rows)
	root_elements<-c()
	if(length(index_candidates)!=0){
	for(i in 1:length(index_candidates)){
		index<-which(d[,index_candidates[i]]==1)
		root_elements[i]<-rows[index]
	}
	
	index_S<-grep("S",root_elements)
	candidates_S<-root_elements[index_S]
	Action<-NULL
	if(length(which(candidates_S=="S2"))==1){
		index_S2<-which(candidates_S=="S2")
		Action<-action_candidates[index_S2]
		index_row<-which(rows==Action)
		}
	candidates_Action<-c()
	if(length(which(candidates_S=="S2"))!=1){ #proveri ubavo so kako
			
	#if(length(which(candidates_S=="S2"))>1){
		candidates_Action<-action_candidates[index_S]
 	}
 	
 	paths<-names(tree$leaves)
	leaves_words<-c()
	if(length(Action)!=0){
 		reg_expression<-paste(Action,".([(VP[0-9]+.|VB[DGNPZ]*[0-9]+.|MD[0-9]+.|ADVP[0-9]+].)+[a-z]+[0-9]+",sep="",collapse="")
 		index<-grep(reg_expression,paths)
 		paths_candidates<-paths[index]
 		if(length(paths_candidates)!=0){
 		for(i in 1:length(paths_candidates)){
 		
 			temp<-strsplit(paths_candidates[i],"\\.")
 			temp<-strsplit(temp[[1]],"[0-9]+")
 			leaves_words[i]<-temp[[length(temp)]]
 			}
 
 		}
 	}
 	
 	leaves_list<-list()
 	if(length(candidates_Action)!=0){
 		for(i in 1:length(candidates_Action)){
 			reg_expression<-paste(candidates_Action[i],".([(VP[0-9]+.|VB[DGNPZ]*[0-9]+.|MD[0-9]+.|ADVP[0-9]+].)+[a-z]+[0-9]+",sep="",collapse="")
 			index<-grep(reg_expression,paths)
 			paths_candidates<-paths[index]
 			leaves_temp<-c()
 			if(length(paths_candidates)!=0){
 				for(j in 1:length(paths_candidates)){
 			
 					temp<-strsplit(paths_candidates[j],"\\.")
 					temp<-strsplit(temp[[1]],"[0-9]+")
 					leaves_temp[j]<-temp[[length(temp)]]
 
	 		}
 		leaves_list[[i]]<-leaves_temp
 		}	
 	}
 	
 }
 
 #extracting of action candidates
		action_final<-rep(0,length(phrases))
		if(length(leaves_words)!=0){
	
			for(k in 1:length(leaves_words)){
				if(leaves_words[k]!=0){
					for(i in 1:length(phrases)){
						if(chunks3[beginnings3[i]]=="B-VP"){
							check<-strsplit(phrases[i]," ")
							ifelse(leaves_words[k] %in% check[[1]], action_final[i]<-action_final[i]+1,action_final[i]<-action_final[i]+0)
						}
						else{
							action_final[i]<-action_final[i]+0
						}
	
					}
				}
				else{
					k<-k+1	
				}
			}
		}
		
		if(length(leaves_list)!=0){
	
			for(k in 1:length(leaves_list)){
				if(length(leaves_list[[k]])!=0){
					for(j in 1:length(leaves_list[[k]])){
						for(i in 1:length(phrases)){
							if(chunks3[beginnings3[i]]=="B-VP"){
								check<-strsplit(phrases[i]," ")
								ifelse(leaves_list[[k]][j] %in% check[[1]], action_final[i]<-action_final[i]+1,action_final[i]<-action_final[i]+0)
							}
							else{
								action_final[i]<-action_final[i]+0
							}
						}
					
					}
					}
					else{
						k<-k+1	
					}
			}
		}

	#additional check ako samo eden u leaves_words e eden are prava problem

	action_final[action_final>=1]<-1
	Action<-action_final[which(action_final==1)]
	Actions<-phrases[which(action_final==1)]

	Actions<-phrases[which(action_final==1)]
	
	unit_final<-B[,3]
	food_final<-B[,1]
	nutrient_final<-B[,2]
	
	temp<-chunks3[beginnings3]
	if(length(which(action_final==1))==1){
	index_action<-which(action_final==1)
	}
	if(length(which(action_final==1))>1){
		index_action<-grep(Actions[1],phrases)
		index_action<-index_action[1]
		}
		if(sum(action_final)>=1){
	subjects<-c()
	for(i in seq(index_action-1,1,-1)){
		if(i!=1){
			if((temp[i]=="B-NP" || temp[i]=="B-ADJP") && (temp[i-1]=="O" || temp[i-1]=="B-NP" || temp[i-1]=="B-SBAR")){
				subjects[i]<-phrases[i]
			}
			else if((temp[i]=="B-NP" || temp[i]=="B-ADJP" || temp[i]=="O") && temp[i-1]=="B-VP"){
				subjects[i]<-phrases[i]
				break
			}
			else if(temp[i]=="B-NP" && (phrases[i-1]=="For" || phrases[i-1]=="for" || phrases[i-1]=="to" || phrases[i-1]=="To")){
				subjects[i]<-phrases[i]
			}
			
		
			
		}
		else {
			if(temp[i]=="B-NP" && temp[i+1]!="B-PP"){
				subjects[i]<-phrases[i]
				break
			}
		}
		
		
		
	}
	

#extract subjects with PP (special)	
	for(i in seq(index_action-1,1,-1)){
		if(i!=1 && i!=2){
			if((food_final[i]==1 || unit_final[i]==1 || nutrient_final[i]==1) && (food_final[i-2]==1 || unit_final[i-2]==1 || nutrient_final[i-2]==1) && temp[i-1]=="B-PP"){
				subjects[i]<-phrases[i]
				subjects[i-2]<-phrases[i-2]
			}
		}
		else
		{
				if((food_final[1]==1 || unit_final[1]==1 || nutrient_final[1]==1) && (food_final[3]==1 || unit_final[3]==1 || nutrient_final[3]==1) && temp[2]=="B-PP"){
					subjects[1]<-phrases[1]
					subjects[3]<-phrases[3]
				}
		}
	}

	Action<-action_final[which(action_final==1)]
	
	
	
	return(list(Actions=Actions,action_final=action_final,subjects=subjects,Action=Action,index_action=index_action))
 	
 	
 	}
 	}





  	
  	

	
}

#Select the food entities
#Inputs: the output from the third chunking, output from the selection of the action entity, phrases


Select_Food<-function(output,output_action,phrases){
	Foods<-c()
	
	chunks3<-output$chunks3
	B<-output$B
	
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("^O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))


	food_final<-B[,1]
	index_foods<-which(food_final==1)

	if(length(output_action)!=0){
	index_action<-output_action$index_action
	
	
	
	if(length(index_foods)!=0){	
		
	counter_foods<-1
	
	if(length(index_foods)!=1){
	temp<-abs(index_foods-index_action)
	index_min<-min(abs(index_foods-index_action))
	if(length(which(temp==index_min))!=1){
	min_food<-which(temp==index_min)[1]
	}
	if(length(which(temp==index_min))==1){
		min_food<-which(temp==index_min)
		}


	correct_food<-index_foods[min_food]
	index_foods<-index_foods[-min_food]
	# if(length(correct_food)!=1){
		# correct_food<-sample(correct_food,1)
	# }
	Foods[counter_foods]<-phrases[correct_food]
	counter_foods<-counter_foods+1
	for(i in 1:length(index_foods)){
		if(index_foods[i]!=correct_food){
			if((index_foods[i]<index_action && correct_food<index_action) || (index_foods[i]>index_action && correct_food>index_action) ){
				difference_food<-index_foods[i]-correct_food
				ifelse(difference_food>0,check<-seq(correct_food+1,index_foods[i],1),check<-seq(index_foods[i],correct_food-1,1))
				if(length(grep("VP",chunks3[beginnings3][check]))==0){
					Foods[counter_foods]<-phrases[index_foods[i]]
					counter_foods<-counter_foods+1
				}
			}
			else{
				#maybe error
				differences_food_action<-index_foods[i]-index_action
				ifelse(differences_food_action>0,check<-seq(index_action+1,index_foods[i],1),check<-seq(1,index_action-1,1))
				if(length(grep("VP",chunks3[beginnings3][check]))==0){
					Foods[counter_foods]<-phrases[index_foods[i]]
					counter_foods<-counter_foods+1
				}


				
			}
		}
	}
	}
	else{
		Foods[counter_foods]<-phrases[index_foods[1]]
		counter_foods<-counter_foods+1
	}
	}
}
if(length(output_action)==0){
	Foods<-phrases[index_foods]
}

	return(Foods)
	

	
}




#Select the unit entities
#Inputs: the output from the third chunking, output from the selection of the action entity, phrases

Select_Unit<-function(output,output_action,phrases){
	Units<-c()
	
	chunks3<-output$chunks3
	B<-output$B
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("^O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))
	
	unit_final<-B[,3]
	index_units<-which(unit_final==1)	
	if(length(output_action)!=0){
	index_action<-output_action$index_action
	
	if(length(index_units)!=0){
		counter_units<-1
		if(length(index_units)!=1){
	temp<-abs(index_units-index_action)
	index_min<-min(abs(index_units-index_action))
	if(length(which(temp==index_min))!=1){
	min_unit<-which(temp==index_min)[1]
	}
	if(length(which(temp==index_min))==1){
		min_unit<-which(temp==index_min)
		}
	
	
	correct_unit<-index_units[min_unit]
	index_units<-index_units[-min_unit]
	Units[counter_units]<-phrases[correct_unit]
	counter_units<-counter_units+1
	for(i in 1:length(index_units)){
		if(index_units[i]!=correct_unit){
			if((index_units[i]<index_action && correct_unit<index_action) || (index_units[i]>index_action && correct_unit>index_action) ){
				difference_unit<-index_units[i]-correct_unit
				ifelse(difference_unit>0,check<-seq(correct_unit+1,index_units[i],1),check<-seq(index_units[i],correct_unit-1,1))
				if(length(grep("VP",chunks3[beginnings3][check]))==0){
					Units[counter_units]<-phrases[index_units[i]]
					counter_units<-counter_units+1
				}
			}
			else{
				differences_unit_action<-index_units[i]-index_action
				ifelse(differences_unit_action>0,check<-seq(index_action+1,index_units[i],1),check<-seq(1,index_action-1,1))
				if(length(grep("VP",chunks3[beginnings3][check]))==0){
					Units[counter_units]<-phrases[index_units[i]]
					counter_units<-counter_units+1
				}


				
			}
		}
	}
	}
	else{
		Units[counter_units]<-phrases[index_units[1]]
		counter_units<-counter_units+1
	}

	}
	}
	if(length(output_action)==0){
	Units<-phrases[index_units]
}
	return(Units)
	
}



#Select the unit entities
#Inputs: the output from the third chunking, output from the selection of the action entity, phrases

Select_Nutrient<-function(output,output_action,phrases){
	
	Nutrients<-c()
	
	chunks3<-output$chunks3
	B<-output$B
	beginnings_chunks31<- grep("^B",chunks3)
	beginnings_chunks32<-grep("^O",chunks3)
	beginnings3<-sort(c(beginnings_chunks31,beginnings_chunks32))


	nutrient_final<-B[,2]
	index_nutrients<-which(nutrient_final==1)
	if(length(output_action)!=0){
		index_action<-output_action$index_action
	if(length(index_nutrients)!=0){
		
		
counter_nutrients<-1
if(length(index_nutrients)!=1){
	temp<-abs(index_nutrients-index_action)
	index_min<-min(abs(index_nutrients-index_action))
	if(length(which(temp==index_min))!=1){
	min_nutrient<-which(temp==index_min)[1]
	}
	if(length(which(temp==index_min))==1){
		min_nutrient<-which(temp==index_min)
		}
	correct_nutrient<-index_nutrients[min_nutrient]
	index_nutrients<-index_nutrients[-min_nutrient]
	Nutrients[counter_nutrients]<-phrases[correct_nutrient]
	counter_nutrients<-counter_nutrients+1
	for(i in 1:length(index_nutrients)){
		if(index_nutrients[i]!=correct_nutrient){
			if((index_nutrients[i]<index_action && correct_nutrient<index_action) || (index_nutrients[i]>index_action && correct_nutrient>index_action) ){
				difference_nutrient<-index_nutrients[i]-correct_nutrient
				ifelse(difference_nutrient>0,check<-seq(correct_nutrient+1,index_nutrients[i],1),check<-seq(index_nutrients[i],correct_nutrient-1,1))
				if(length(grep("VP",chunks3[beginnings3][check]))==0){
					Nutrients[counter_nutrients]<-phrases[index_nutrients[i]]
					counter_nutrients<-counter_nutrients+1
				}
			}
			else{
				differences_nutrient_action<-index_nutrients[i]-index_action
				ifelse(differences_nutrient_action>0,check<-seq(index_action+1,index_nutrients[i],1),check<-seq(1,index_action-1,1))
				if(length(grep("VP",chunks3[beginnings3][check]))==0){
					Nutrients[counter_nutrients]<-phrases[index_nutrients[i]]
					counter_nutrients<-counter_nutrients+1
				}


				
			}
		}
		}
	}
	else{
		Nutrients[counter_nutrients]<-phrases[index_nutrients[1]]
		counter_nutrients<-counter_nutrients+1
	}

		
	}
}
if(length(output_action)==0){
	Nutrients<-phrases[index_nutrients]
}

	return(Nutrients)
}


#Select the Group entities
#Inputs: output from the third additional chunking and output from the action entuty selection

Select_Group<-function(output,output_action){
	Groups<-c()
	if(length(output_action)!=0){
	subjects<-output_action$subjects
	B<-output$B
	food_final<-B[,1]
	nutrient_final<-B[,2]
	unit_final<-B[,3]
	counter_groups<-1
	if(length(subjects)!=0){
	for(i in 1:length(subjects)){
		if(food_final[i]==0 && nutrient_final[i]==0 && unit_final[i]==0 && !is.na(subjects[i])){
			Groups[counter_groups]<-subjects[i]
			counter_groups<-counter_groups+1
		}	

	
	}
	}
	}
	return(Groups)
}



sentences<-convert_text_to_sentences(paragraphs[19])
Foods<-list()
Units<-list()
Nutrients<-list()
Groups<-list()
Actions<-list()
	

for(i in 1:length(sentences)){
	
	s<-sentences[i]
	s<-Pre_processing(s)
	df_pos<-POStagging(s)
	df_chunks<-Default_chunking(s)
	sentences2<-Split_ADVP(df_pos,df_chunks,s)
	
	if(length(sentences2)==0){
		
		A<-Chunk_matrix(df_chunks,df_pos)
		food<-Food_checking(s,df_pos)
		nutrient<-Nutrient_checking(df_pos,s)
		unit<-Unit_checking(df_pos,abbreviations,metric_kitchen)
		C<-Entities_matrix(food,unit,nutrient)
		B<-Candidates_matrix(A,C)
		chunks1<-First_chunking(df_chunks,B)
		chunks2<-Second_chunking(chunks1,df_pos,food,unit,nutrient)
		third<-Third_chunking(chunks2,df_pos,food,unit,nutrient)
		phrases<-Phrases(third,df_pos)
		output_action<-Select_Action_Subjects(s,phrases,third)
		Foods[[i]]<-Select_Food(third,output_action,phrases)
		Nutrients[[i]]<-Select_Nutrient(third,output_action,phrases)
		Units[[i]]<-Select_Unit(third,output_action,phrases)
		Groups[[i]]<-Select_Group(third,output_action)
		if(length(output_action)!=0){
			Actions[[i]]<-phrases[output_action$index_action]
		}
		else{
			Actions[[i]]<-""
		}
		
		}
		
		if(length(sentences2)!=0){
			Foods_temp<-list()
			Units_temp<-list()
			Nutrients_temp<-list()
			Groups_temp<-list()
			Actions_temp<-list()
			for(j in 1:length(sentences2)){
				s<-sentences2[j]
				if(s!="."){
				df_pos<-POStagging(s)
				df_chunks<-Default_chunking(s)
				A<-Chunk_matrix(df_chunks,df_pos)
				food<-Food_checking(s,df_pos)
				nutrient<-Nutrient_checking(df_pos,s)
				unit<-Unit_checking(df_pos,abbreviations,metric_kitchen)
				C<-Entities_matrix(food,unit,nutrient)
				B<-Candidates_matrix(A,C)
				chunks1<-First_chunking(df_chunks,B)
				chunks2<-Second_chunking(chunks1,df_pos,food,unit,nutrient)
				third<-Third_chunking(chunks2,df_pos,food,unit,nutrient)
				phrases<-Phrases(third,df_pos)
				output_action<-Select_Action_Subjects(s,phrases,third)
				Foods_temp[[j]]<-Select_Food(third,output_action,phrases)
				Nutrients_temp[[j]]<-Select_Nutrient(third,output_action,phrases)
				Units_temp[[j]]<-Select_Unit(third,output_action,phrases)
				Groups_temp[[j]]<-Select_Group(third,output_action)
				if(length(output_action)!=0){
					Actions_temp[[j]]<-phrases[output_action$index_action]
				}
				else{
					Actions_temp[[j]]<-""
				}
		}

				
			}
			
			Foods[[i]]<-unlist(Foods_temp)
			Nutrients[[i]]<-unlist(Nutrients_temp)
			Units[[i]]<-unlist(Units_temp)
			Groups[[i]]<-unlist(Groups_temp)
			Actions[[i]]<-unlist(Actions_temp)
			
			
			
			
			
			}
		
		
}



gsub("(\\w)-(\\w)","\\1 \\2",sentences[1])

#some preprocesing, needs to be done on each sentence before splitting


Pre_processing<-function(s){
	s<-gsub("\"","",s)
	s<-gsub("( |)\\(e.g.,",",",s)
	s<-gsub("\\)\\.",".",s)
	s<-gsub("\\) ",", ",s)
	s<-gsub(" \\(",", ",s)
	s<-gsub("\\(","",s)
	s<-gsub("\\)\\.",".",s)
	s<-gsub("\\.\\)",".",s)
	s<-gsub("ALA.","ALA",s)
	return(s)
}