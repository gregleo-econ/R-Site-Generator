library(fs)



headHtml <- '
    <!DOCTYPE html>
    <html>
    <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
    <title>
    $TITLE
    </title>
    <style type="text/css">
    $STYLE
    </style>
    </head>
    <body>
    '

footHtml <- '
  </body>
  </html>
'



myKnitToHtml <- function (input,output,stylesheet,title) {
  htmlText <- c(headHtml,input,footHtml,collapse="")
  print(title)
  replacements <- list(
    c("^### (.*)$","\n<h3>\\1</h3><BR>"),
    c("^## (.*)$","<h2>\\1</h2><BR>"),
    c("^# (.*)$","<h1>\\1</h1><BR>"),
    c("```","<pre>"),
    c("'''","</pre>"),
    c("`(.*?)`","<code>\\1</code>"),
    c("\\*\\*(.*?)\\*\\*","<b>\\1</b>"),
    c("\\*(.*)\\*","<i>\\1</i>"),
    c("\\[\\!(.*?)\\]\\((.*?)\\)\\]\\((.*?)\\)","<a href='\\3'><img src='\\2' alt='\\1'></a>"),
    c("\\!\\[(.*?)\\]\\((.*?)\\)","<img alt='\\1' src='\\2'>"),
    c("[^!]?\\[(.*?)\\]\\((.*?)\\)"," <a href='\\2'>\\1</a>"),
    c("  $","<BR>"),
    c("\\$TITLE",title),
    c("\\$STYLE",readChar(stylesheet,file.info(stylesheet)$size))  
  )
  
  for(replacement in replacements){
    replacement <- unlist(replacement)
    htmlText <- gsub(replacement[1],replacement[2],htmlText)
  }
  
  
  
  
  write(htmlText,output)
}

list.dirs <- function(path = ".",pattern = NULL,all.dirs = FALSE,full.names = FALSE,ignore.case = FALSE) {
  all <- list.files(path,pattern,all.dirs,full.names = TRUE,recursive = FALSE,ignore.case)
  dirs <- all[file.info(all)$isdir]
  if (isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}

#Get Folders in Markdown Directory
getFolders <- function(directory) {
  markdownDirectory = paste(c(directory, "/markdown"), collapse = "")
  folders = list.dirs(markdownDirectory)
}

#Get Files in Markdown Directory Folder
getFiles <- function(folder, directory) {
  markdownDirectory = paste(c(directory, "/markdown"), collapse = "")
  staticDirectory = paste(c(directory, "/static"), collapse = "")
  currentDirectoryMarkdown = paste(c(markdownDirectory, "/", folder), collapse ="")
  currentDirectory = paste(c(staticDirectory, "/", folder), collapse ="")
  temp = list.files(currentDirectoryMarkdown, pattern = "*.md$")
}

#Extract Page Title from File
getTitle <- function(fileMarkdownPath) {
  content <- readLines(fileMarkdownPath,n=3)
  titleLine <- content[3]
  pageTitle <- substring(titleLine, 3) 
  pageTitle
}

getPriority <- function(fileMarkdownPath){
  content <- readLines(fileMarkdownPath,n=1)
  priorityLine <- content[1]
  priority <- substring(priorityLine, 2)
  as.numeric(priority)
}

#Setup the Static Folder
makeStaticFolder <- function(folder, directory) {
  staticDirectory = paste(c(directory, "/static"), collapse = "")
  currentDirectory = paste(c(staticDirectory, "/", folder), collapse =
                             "")
  dir.create(currentDirectory)
}

setupStatic <- function(directory, folders) {
  sapply(folders, makeStaticFolder, directory = directory)
}

#Setup the Temp Folder
setupTemp <- function(directory) {
  tempDirectory = paste(c(directory, "/temp"), collapse = "")
  dir.create(tempDirectory)
}

#Get Full Path to Markdown File
getMarkdownPath <- function(file, folder, directory) {
  paste(c(directory, "/markdown/", folder, "/", file), collapse = "")
}

#Get Full Path to Static File
getStaticPath <- function(file, folder, directory) {
  paste(c(
    directory,
    "/static/",
    folder,
    "/",
    substr(file, 1, nchar(file) - 2),
    "html"
  ),
  collapse = "")
}



#Make a Link to the Static File in Markdown Format
makeLink <- function(fileData) {
  paste(c(
    "[",
    fileData$title,
    "](",
    fileData$folder,
    "/",
    substr(as.character(fileData$fileName), 1, nchar(as.character(fileData$fileName)) - 2),
    "html)<BR>\n"
  ),
  collapse = "")
}


#Create the Index Page
makeIndex <- function(fileData,pageTitle,indexHeader) {
  indexHeaderPath <-  paste(directory, "/markdown/indexHeader.md", sep = "")
  indexHeader <-  readChar(indexHeaderPath, file.info(indexHeaderPath)$size)
  indexLines <- indexHeader
  folders <- unique(fileData$folder)
  for(currentFolder in folders){
    folderName <- substring(currentFolder,3)
    folderName <- gsub("_"," ",folderName,fixed=TRUE)
    indexLines <- c(indexLines,paste("## ",toupper(folderName),sep=""))
    folderHeaderPath <-  paste(directory,"/markdown/",currentFolder, "/header.md", sep = "")
    folderHeader <-  readChar(folderHeaderPath, file.info(folderHeaderPath)$size)
    if(nchar(folderHeader)>0){indexLines <- c(indexLines,folderHeader,"<BR>")}
    subsetFiles <-  fileData[which(fileData$folder==currentFolder),]
    
    subsetFiles <- subsetFiles[order(subsetFiles$priority,decreasing = TRUE),]
    
    for(i in 1:dim(subsetFiles)[1]){
      indexLines <- c(indexLines,makeLink(subsetFiles[i,]))
    }
  }
  timeStamp <- paste0("```",timestamp(),"```")
  indexLines <- c(indexLines,timeStamp)
  
  myKnitToHtml(
    indexLines,
    paste(directory, "/static/index.html", sep = ""),
    stylesheet = style,
    title=pageTitle
  )
}

#Create the Index Page



#Create an HTML Page from Markdown File
makeHtml <- function(fileData,style) {
  
  content <- readLines(fileData$fullMarkdownPath)[-1]
  content[1] <- "[Back](../index.html)<BR>"
  myKnitToHtml(content, output=fileData$fullStaticPath, stylesheet = style,title=fileData$title)
}






#Parse the files and make a dataframe with relevant file info. 
makeFileData <- function(directory){
  
  fileName=c()
  folder=c()
  fullMarkdownPath=c()
  fullStaticPath=c()
  title=c()
  priority=c()
  
  folders <- getFolders(directory)
  
  for (fileFolder in folders) {
    files <- getFiles(fileFolder, directory)
    
    for(file in files){
      
      if(file == "header.md"){}
      
      else{
        fileName <- c(fileName,file)
        folder <- c(folder,fileFolder)
        
        fileMarkdownPath <- getMarkdownPath(file,fileFolder,directory)
        fullMarkdownPath <- c(fullMarkdownPath,fileMarkdownPath)
        
        fileStaticPath <- getStaticPath(file,fileFolder,directory)
        fullStaticPath <- c(fullStaticPath,fileStaticPath)
        
        fileTitle <- getTitle(fileMarkdownPath)
        title <- c(title,fileTitle)
        
        filePriority <- getPriority(fileMarkdownPath)
        priority <- c(priority,filePriority)
      }
      
    }
  }
  return(data.frame(fileName = fileName,folder = folder,fullMarkdownPath = fullMarkdownPath,fullStaticPath = fullStaticPath,title = title,priority = priority,stringsAsFactors=FALSE))
}


#Make sure the directory structure of the site is ok. 
setupSite <- function(directory){
  setwd(directory)
  dir.create(paste(c(directory, "/static/"), collapse = ""))
  folders <- getFolders(directory)
  setupTemp(directory)
  setupStatic(directory, folders)
}


#Put it All Together to Make Page
makePage <- function(directory, style,pageTitle,indexHeader) {
  print("Setting Up Site")
  setupSite(directory)
  print("Getting File Data")
  fileData <- makeFileData(directory)
  
  for(i in 1:dim(fileData)[1]){
    
    print(paste0("Setting up Page ",fileData[i,]$title))
    makeHtml(fileData[i,],style)
  }
  print("Making Index")
  makeIndex(fileData,pageTitle)
}

directory = getwd()


style = paste(c(directory,"/style/style.css"),collapse="")
makePage(directory,style,"Page Title")


