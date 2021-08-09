library(shinyMatrix)
mcchoices=c("yes , no", 
            "lower ,not equal to , higher , can not say", 
            "lower , not equal to , higher", 
            "is statistically significant , is not statistically significant", 
            "is statistically significant , is not statistically significant , can not say", 
            "is , is not", 
            "Male , Female",
            "true , false",
            "has , does not have",
            "!= , < , >",
            "<,>", "+,-", 
            "&mu; , &pi; , &sigma; , &lambda; , &rho; , other")
names(mcchoices)=mcchoices
names(mcchoices)[11]="\u03BC / \u03C0 / \u03C3 / \u03BB / \u03C1 / other"

all.inputs=list("radio"=c("dtl", "addgraph", "doquiz", paste0("question",1:20,"newline"),
                  paste0("answertype",1:20), "showdata"),
    "text"=c("quizname","category","numquiz","folder", "n", "Ufrom","Uto", "Nmean", "Nstd", 
             "Balpha", "Bbeta","Galpha", "Gbeta", "pc", "pr","cat2varnames", "BNmeans", 
             "BNstds","BNcor","BNnames", paste0("numpoints",1:20), paste0("numprecision",1:20),
             paste0("mcoptions", 1:20)),
    "action"="xmlbutton",
    "area"=c("comments", "RCode", "htxt", paste0("qtxt",1:20), paste0("atxt",1:20),
             paste0("calculate.answer", 1:20), "graphcommand", "gencalc"),
    "select"=c("distribution","ndigit", "srt",paste0("mcoptions", 1:20,"a")))
    
shinyServer(function(input, output, session) {

   output$defineInputs=renderUI({
      n=as.numeric(input$numquestions)
      out=as.list(1:n)
      for(i in 1:n)
        out[[i]] =tagList(
          HTML("<hr>"),
          HTML(paste("<h4>Question ", i, "</h4>")),
          radioButtons(paste0("question",i,"newline"), "Start on new Line", choices=c("Yes", "No"), inline=TRUE),
          textAreaInput(paste0("qtxt",i), "",  value="", width="100%"),
          radioButtons(paste0("answertype",i), "Type of Answer", choices=c("Numeric Answer", "Multiple Choice", "Numeric Matrix", "Verbatim"), inline = TRUE),
          conditionalPanel( condition = paste0('input.answertype', i, '== "Numeric Answer"|input.answertype', i, '== "Numeric Matrix"'),
                            column(3, textInput(paste0("numpoints",i), "Point(s)", value="100", width = "50%")),
                            column(3, textInput(paste0("numprecision",i), "Precision(s)", value="0", width = "50%"))
          ),
          conditionalPanel( condition = paste0('input.answertype', i,  '== "Multiple Choice"'),
                            column(3, textInput(paste0("mcoptions",i), "Choices", placeholder="Yes,No,Maybe")),
                            column(3, selectInput(paste0("mcoptions",i,"a"), "Choices (List)", choices = mcchoices))
          ), 
          textAreaInput(paste0("calculate.answer",i), "Calculations for Questions", placeholder=""),    
          textAreaInput(paste0("atxt",i), "Calculations for Correct Answers", placeholder=" ", width="100%"),
          
          )
      out
   })
   
   observeEvent(input$xmlbutton,{
      input.values=reactiveValuesToList(input)
      input.values[["catpmatrix"]] = input$catpmatrix
      dump("input.values", paste0(folder(),"/",input$quizname,".dta"))
      
   })

   observeEvent(input$readbutton,{
        if(input$moodleRquiz!=" ") {
            input.values=get(input$moodleRquiz)
        }    
        else {    
          fl=paste0(folder(),"/", input$quizname,".dta")
          if( !file.exists(fl) ) return(NULL)
          source(fl)
        } 
        updateNumericInput(session, "numquestions", value=input.values[["numquestions"]]) 
        for(i in 1:length(input.values)) { 
          if(names(input.values)[i]%in%all.inputs[["radio"]])     
             updateRadioButtons(session, names(input.values)[i],  selected = input.values[[i]])
          if(names(input.values)[i]%in%all.inputs[["text"]])     
             updateTextInput(session, names(input.values)[i],  value = input.values[[i]])
          if(names(input.values)[i]%in%all.inputs[["area"]])     
             updateTextAreaInput(session, names(input.values)[i],  value = input.values[[i]])
          if(names(input.values)[i]%in%all.inputs[["select"]])     
             updateSelectInput(session, names(input.values)[i],  selected = input.values[[i]])                 
             
        }
        new.inputs=reactiveValuesToList(input) 

   })        
   
   observeEvent(input$readbutton,{
     if(input$distribution!="Categorical Variable") return(NULL)
     if(input$moodleRquiz!=" ") {
        input.values=get(input$moodleRquiz)
     }   
     else {    
        fl=paste0(folder(),"/", input$quizname,".dta")
        if( !file.exists(fl) ) return(NULL)
        source(fl)
     }
     updateMatrixInput(session, "catpmatrix",  value = input.values[["catpmatrix"]])

   })   
      
   A=reactive({
     cnames = strsplit(input$pc,",")[[1]]
     rnames = strsplit(input$pr,",")[[1]]
     nr=ifelse(length(rnames)<2, 1, length(rnames))
     A=matrix(0, nr, length(cnames))
     if(nr==1) rownames(A)= "p"
     else rownames(A) = rnames
     colnames(A)=cnames
     A
   })
   
   output$catvarinfo=renderUI({
     if(input$distribution != 'Categorical Variable') return(NULL)
     matrixInput("catpmatrix", "", A())
   })
   
   get.info=reactive({
     input.values <- reactiveValuesToList(input)
     input.values$question1newline="No"
     out=as.list(1:input$numquestions)
     for(i in 1:input$numquestions) {
        calc.answer=strsplit(input.values[[paste0("calculate.answer", i)]],"\n")[[1]]
        if(length(calc.answer)>1) {
              extra.calc=calc.answer[-length(calc.answer)]
              calc.answer=calc.answer[length(calc.answer)]
        }
        else extra.calc=NULL
        if(i==input$numquestions & input$addgraph=="Yes") {
            graphcommand=strsplit(input.values[["graphcommand"]],"\n")[[1]] 
            extra.calc  = c(extra.calc, graphcommand, "plt64=png64(plt)")
        }
        if(input.values[[paste0("mcoptions", i)]]=="")
           options=strsplit(input.values[[paste0("mcoptions", i,"a")]],",")[[1]]
        else
           options=strsplit(input.values[[paste0("mcoptions", i)]],",")[[1]]
        if(input.values[[paste0("atxt",i)]]=="")    
           input.values[[paste0("atxt",i)]]=input.values[[paste0("qtxt",i)]]
        out[[i]]=list(qtxt=strsplit(input.values[[paste0("qtxt", i)]],"@")[[1]],
                   atxt=strsplit(input.values[[paste0("atxt", i)]],"@")[[1]],
                   calc.answer=calc.answer,
                   extra.calc=extra.calc,
                   answertype=input.values[[paste0("answertype", i)]],
                   questionnewline=input.values[[paste0("question", i,"newline")]],
                   options=options,
                   numpoints=strsplit(input.values[[paste0("numpoints", i)]], ",")[[1]],
                   numprecision=strsplit(input.values[[paste0("numprecision", i)]], ",")[[1]]
                 )  
         names(out[[i]])=c("qtxt", "atxt", "calc.answers", "extra.calc", "answertype",  "questionnewline", 
                         "options", "numpoints", "numprecision")
     }
     out 
   })
  
   par.code=function(a, nme) {
        a = as.numeric(strsplit(a, ",")[[1]])
        if(length(a)==1) ln = paste(nme, "=", a)
        else
           ln = paste0(nme, "=", a[1], "+", a[3], "*", "sample(0:", ((a[2]-a[1])/a[3]), ", 1)") 
        ln
   }

   folder = reactive({
     if(input$folder!="getwd()") {
        if(!dir.exists(input$folder)) return(NULL)
        return(input$folder)
     }
     folder=strsplit(getwd(), "/")[[1]]
     paste0(folder[1:(length(folder)-1)],collapse="/")
   })           
   
   gen.R = eventReactive(input$xmlbutton,{ 
        txt = paste0(input$quizname,"=function() {")
        txt[length(txt)+1] = paste0("category=\"",input$category,"\"")
        txt[length(txt)+1] = "quizname=\"problem -\""
        if(input$distribution!="No Data") {
          txt[length(txt)+1] = par.code(input$n, "n")
          txt = c(txt, gen.data.commands())
          if(input$srt=="Yes" & input$distribution !="Bivariate Normal") txt[length(txt)+1]="x=sort(x)"      
          if(input$distribution != "Categorical Variable") 
             if(input$distribution != "R Code")  
                txt[length(txt)+1]=paste0("x=round(x, ", as.numeric(input$ndigit),")")        
        }      
        txt = c(txt, strsplit(input$gencalc, "\n")[[1]])
        txt[length(txt)+1] = paste0("res=as.list(1:", input$numquestions, ")")  
        for(i in 1:input$numquestions) 
            txt = c(txt, get.info()[[i]]$extra.calc, paste0("res[[", i, "]]= ", get.info()[[i]]$calc.answer))         
        txt[length(txt)+1] = paste("qtxt = ", qtxt())
        txt[length(txt)+1] = paste("atxt = ", atxt())     
        txt[length(txt)+1] = paste("htxt = \"", input$htxt,"\"")        
        tmp = "list(qtxt = paste0(\"<h5>\", qtxt, \"</h5>\""
        if(input$addgraph=="Yes") 
           tmp = paste0(tmp, ",  plt64")   
        if(input$showdata=="yes")    
           if(input$distribution!="No Data")    
             tmp = paste0(tmp, ", moodle.table(x)")            
        txt[length(txt)+1] = paste0(tmp, "),")  
        txt[length(txt)+1] = "    htxt = paste0(\"<h5>\", htxt, \"</h5>\")," 
        txt[length(txt)+1] = "    atxt = paste0(\"<h5>\", atxt, \"</h5>\"),"                              
        txt[length(txt)+1] = "    category = category, quizname = quizname)"
        txt[length(txt)+1] = "}"  
        txt[2:(length(txt)-1)]=paste0("   ",txt[2:(length(txt)-1)]) 
        txt              
   })
   

        
   gen.data.commands = reactive({
       command=NULL
       if(input$distribution=="Uniform") {       
           command=c(par.code(input$Ufrom, "from"), par.code(input$Uto, "to"), "x=runif(n, from, to)")
       }    
       if(input$distribution=="Normal") {
           command=c(par.code(input$Nmean, "m"), par.code(input$Nstd, "s"), "x=rnorm(n, m, s)")
       }    
       if(input$distribution=="Beta") {
           command=c(par.code(input$Balpha, "alpha"), par.code(input$Bbeta, "beta"), "x=rbeta(n, alpha, beta)")
       }    
       if(input$distribution=="Gamma") {
           command=c(par.code(input$Galpha, "alpha"), par.code(input$Gbeta, "beta"), "x=rgamma(n, alpha, beta)")
       }    
       if(input$distribution=="Categorical Variable") {
           cnames = strsplit(input$pc,",")[[1]]
           rnames = strsplit(input$pr,",")[[1]]
           if(length(rnames)<2) {
             vals=paste0("c(\"", paste0(cnames, collapse="\",\""), "\")")           
             probs=input$catpmatrix
             probs=paste0("c(", paste0(probs, collapse=","), ")")
             command=paste0("x=sample(",vals, ", size=n, replace=TRUE, prob=", probs,")")
           }
           else {
             varnames=strsplit(input$cat2varnames,",")[[1]]
             ln1 = paste0("p=matrix(c(", paste0(input$catpmatrix,collapse=","), "),", length(rnames), ",", length(cnames),")")  
             cnames=paste0("c(\"", paste0(cnames, collapse="\",\""), "\")")           
             rnames=paste0("c(\"", paste0(rnames, collapse="\",\""), "\")")           
             ln2 = paste0("dimnames(p)=list(", rnames,",", cnames,")")
             ln3=paste0("x=rcategorical(n, p)")
             ln4=paste0("colnames(x)=", paste0("c(\"", paste0(varnames, collapse="\",\""), "\")") )
             command=c(ln1, ln2, ln3, ln4)
           }
           command  
       }     
       if(input$distribution=="R Code") {
          command=strsplit(input$RCode,"\n")[[1]]
       }
       if(input$distribution=="Bivariate Normal") {
           library(mvtnorm)
           nms=strsplit(input$BNnames,",")[[1]]
           mu=as.numeric(strsplit(input$BNmeans,",")[[1]])
           s=as.numeric(strsplit(input$BNstds,",")[[1]])
           cr=as.numeric(input$BNcor)   
           ln="library(mvtnorm)"
           ln[2] = paste0("mu=c(", mu[1], ", ", mu[2], ")")
           ln[3] = paste0("sigma=matrix(c(", s[1]^2, ", ", s[1]*s[2]*cr, ", ", s[1]*s[2]*cr, ", ", s[2]^2,"), 2, 2)")
           ln[4] = "x=rmvnorm(n, mu, sigma)"
           ln[5] = paste0("colnames(x) = c(\"", nms[1], "\",\"", nms[2], "\")")
           command=ln
       } 
       command
   })
   
  
   qtxt = eventReactive(input$xmlbutton,{
        starttext=rep("", input$numquestions)
        endtext=rep("", input$numquestions)
        question=rep("", input$numquestions)
        for(i in 1:input$numquestions) { 
            starttext[i]= get.info()[[i]]$qtxt[1]
            if(get.info()[[i]]$questionnewline=="Yes") starttext[i]=paste0("<p>",starttext[i])
            if(length(get.info()[[i]]$qtxt)>1) endtext[i]=get.info()[[i]]$qtxt[2]
            if(get.info()[[i]]$answertype=="Numeric Answer")   {
                w=get.info()[[i]]$numpoints
                if(length(w)>1) w=paste0("c(", paste0(w, collapse=","),")")
                eps=get.info()[[i]]$numprecision
                if(length(eps)>1) eps=paste0("c(", paste0(eps, collapse=","),")")
                question[i] = paste0("moodlequizR::nm(res[[", i, "]], w = ", w , ", eps = ", eps, ")")
            } 
            if(get.info()[[i]]$answertype=="Multiple Choice")   { 
              m=length(get.info()[[i]]$options)
              options=paste0("\"", get.info()[[i]]$options,"\"")
              options=paste0("c(", paste0(options, collapse=","),")") 
              question[i] = paste0("moodlequizR::mc(", options,  ", ifelse(1:", m, "== res[[", i, "]], 100, 0))$qmc")
            }
            if(get.info()[[i]]$answertype=="Numeric Matrix") {
                w=get.info()[[i]]$numpoints
                if(length(w)>1) w=paste0("c(", paste0(w, collapse=","),")")
                eps=get.info()[[i]]$numprecision
                if(length(eps)>1) eps=paste0("c(", paste0(eps, collapse=","),")")             
                question[i] = paste0("moodlequizR::qamatrix(res[[", i, "]], points=", w, ", precision=", eps, ")$qtxt")
            }
            if(get.info()[[i]]$answertype=="Verbatim")   {     
              question[i] = paste0(" res[[", i, "]]")
            }                       
        }
        code = paste0(starttext, " \",", question, ",\" ", endtext, collapse="")
        code = paste0("\"<p>", code, "</p>\"")
        code = paste("paste0(", code, ")")   
        code
   })

   atxt = eventReactive(input$xmlbutton,{
        starttext=rep("", input$numquestions)
        endtext=rep("", input$numquestions)
        answer=rep("", input$numquestions)
        for(i in 1:input$numquestions) {
            starttext[i]= get.info()[[i]]$atxt[1] 
            if(get.info()[[i]]$questionnewline=="Yes") starttext[i]=paste0("<p>",starttext[i])  
            if(length(get.info()[[i]]$atxt)>1) endtext[i]=get.info()[[i]]$atxt[2]
            if(get.info()[[i]]$answertype=="Numeric Answer")   
                answer[i] = paste0(" res[[", i, "]]")
            if(get.info()[[i]]$answertype=="Numeric Matrix") 
                answer[i] = paste0("moodlequizR::qamatrix(res[[", i, "]])$atxt")
            if(get.info()[[i]]$answertype == "Multiple Choice") { 
                m=length(get.info()[[i]]$options)
                options=paste0("\"", get.info()[[i]]$options,"\"")
                options=paste0("c(", paste0(options, collapse=","),")")
                answer[i] = paste0( options, "[res[[", i, "]]]")
            }   
            if(get.info()[[i]]$answertype=="Verbatim")   {
              if(starttext[i]=="NO ANSWER") {
                starttext[i]=""
                answer[i]="\"\""
              }  
              else      
                answer[i] = paste0(" res[[", i, "]]")
            }            
        }
        code = paste0(starttext, " \",", answer, ",\" ", endtext, collapse="")
        code = paste0("\"<p>", code, "</p>\"")
        code = paste("paste0(", code, ")")
        code
   })
   
   output$messages=renderText({
       out=""
       if(input$folder=="getwd()") return(out)
       if(!dir.exists(input$folder)) out="Folder path incomplete or folder does not exist"
       out
   })
    
   output$text <- renderPrint({                   
        txt = gen.R()
        write(txt, paste0(folder(), "/", input$quizname,".R"))
        source(paste0(folder(), "/", input$quizname, ".R"))
        fun=get(input$quizname)
        if(input$doquiz=="Yes")
           moodlequizR::genquiz(as.numeric(input$numquiz), fun, folder = folder())     
        cat("Files are save in folder: ", folder(),"\n")
        cat(paste0(input$quizname,".R:\n"))
        fun
   })
})     
