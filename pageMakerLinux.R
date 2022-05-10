library(knitr)
library(fs)
library(stringr)
library(dplyr)
library(stringi)
library(magrittr)
library(txtplot)
library(gtrendsR)

list.dirs <- function(path = ".",
                      pattern = NULL,
                      all.dirs = FALSE,
                      full.names = FALSE,
                      ignore.case = FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path,
                    pattern,
                    all.dirs,
                    full.names = TRUE,
                    recursive = FALSE,
                    ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
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
  currentDirectoryMarkdown = paste(c(markdownDirectory, "/", folder), collapse =
                                     "")
  currentDirectory = paste(c(staticDirectory, "/", folder), collapse =
                             "")
  temp = list.files(currentDirectoryMarkdown, pattern = "*.md$")
  temp
}

#Extract Page Title from File
getTitle <- function(file) {
  pageTitle <- str_extract(readChar(file, 500), regex("# .*"))
  pageTitle <- substring(pageTitle, 3)
  pageTitle
}


#Extract Page Title from File
getPriority <- function(file) {
  priority <- str_extract(readChar(file, 10), regex("[0-9]{1,10}"))
  priority
}


#Add a "back" link to each page.
addBack <- function(fileInfo){
  content <- fileInfo$content
  content <- paste("[Back](../index.html)  \n\n\n", content, collapse = "")
  fileInfo$content <- content
  fileInfo
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

#Setup the Temp Folder
setupGopher <- function(directory) {
  gopherDirectory = paste(c(directory, "/gopher"), collapse = "")
  dir.create(gopherDirectory)
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
makeLink <- function(fileInfo) {
    paste(c(
    "[",
    fileInfo$title,
    "](",
    fileInfo$folder,
    "/",
    substr(as.character(fileInfo$fileName), 1, nchar(as.character(fileInfo$fileName)) - 2),
    "html)\n"
  ),
  collapse = "")
}


#Create the Index Page
makeIndex <- function(fileData,pageTitle,indexHeader) {
  
  indexHeaderPath <-  paste(directory, "/markdown/indexHeader.md", sep = "")
  indexHeader <-  readChar(indexHeaderPath, file.info(indexHeaderPath)$size)
  indexText <- indexHeader
  folders <- fileData %>% pull(folder) %>% unique
  for(currentFolder in folders){
    folderName <- substring(currentFolder,3)
    folderName <- str_replace(folderName,"_"," ")
    indexText <- c(indexText,paste("## ",toupper(folderName),sep=""))
    subsetFiles <- fileData %>% filter(folder==currentFolder) %>% arrange(desc(priority))
    for(i in 1:dim(subsetFiles)[1]){
      print(subsetFiles[i,])
      indexText <- c(indexText,makeLink(subsetFiles[i,]))
    }
  }
  indexText <- c(indexText,"```",timestamp(),"```")
  writeLines(indexText, paste(directory, "/markdown/index.md", sep = ""))
  knit2html(
    paste(directory, "/markdown/index.md", sep = ""),
    paste(directory, "/static/index.md", sep = ""),
    stylesheet = style,
    title=pageTitle
  )
}

#Create the Index Page
makeGophermap <- function(fileData,pageTitle,indexHeader) {
  
  gopherHeaderPath <-  paste(directory, "/markdown/gopherHeader.md", sep = "")
  gopherHeader <-  readChar(gopherHeaderPath, file.info(gopherHeaderPath)$size)
  gopherText <- gopherHeader
  
  terms = c("gopher","gemini")
  
  for(term in terms){
    df <-gtrends(term,time="all",onlyInterest = TRUE)
    gopherText <- c(gopherText,paste0("$$ Interest in ",term," over time. $$"))
    gopherText <- c(gopherText,capture.output(txtplot(as.numeric(df$interest_over_time[,1]),df$interest_over_time[,2],ylab="Interest",xlab="Unix Epoch on January 1st, 1970 at UTC.")))
    gopherText <- c(gopherText," ")
  }

  writeLines(gopherText, paste(directory, "/gopher/gophermap", sep = ""))
}

#Add a "back" link to each page.
addBack <- function(fileInfo){
  headerPath <- paste(directory, "/markdown/header.md", sep = "")
  header <- readChar(headerPath, file.info(headerPath)$size)
  content <- fileInfo$content
  content <- paste(header, content, collapse = "")
  fileInfo$content <- content
  fileInfo
}

#Create an HTML Page from Markdown File
makeHtml <- function(fileInfo,style,directory) {
  message("Making",fileInfo$title)
  fileInfo <- addBack(fileInfo)
  tempFile <- paste(directory, "/temp/temp.md", sep = "")
  writeLines(fileInfo$content, tempFile, sep = "")
  knit2html(tempFile, output=fileInfo$fullStaticPath, stylesheet = style)
  setwd(directory)
}




#Remove Priority String
removePriority <- function(content){
  content <- str_remove(content,"[+][0-9]{1,10}")
  content
}


#Parse the files and make a dataframe with relevant file info. 
makeFileData <- function(directory){
  fileName=c()
  folder=c()
  fullMarkdownPath=c()
  fullStaticPath=c()
  title=c()
  content=c()
  priority <- c()
  folders <- getFolders(directory)
  for (fileFolder in folders) {
    files <- getFiles(fileFolder, directory)
    for(file in files){
      fileMarkdownPath <- getMarkdownPath(file,fileFolder,directory)
      fileStaticPath <- getStaticPath(file,fileFolder,directory)
      fileTitle <- getTitle(fileMarkdownPath)  
      filePriority <- as.numeric(getPriority(fileMarkdownPath))
      fileContent <- readChar(fileMarkdownPath, file.info(fileMarkdownPath)$size)
      fileContent <- removePriority(fileContent)
      priority <- c(priority,filePriority)
      fileName <- c(fileName,file)
      folder <- c(folder,fileFolder)
      fullMarkdownPath <- c(fullMarkdownPath,fileMarkdownPath)
      fullStaticPath <- c(fullStaticPath,fileStaticPath)
      title <- c(title,fileTitle)
      content <- c(content,fileContent)
    }
  }
  return(data.frame(fileName = fileName,folder = folder,fullMarkdownPath = fullMarkdownPath,fullStaticPath = fullStaticPath,title = title,priority = priority,content = content,stringsAsFactors=FALSE))
}


#Make sure the directory structure of the site is ok. 
setupSite <- function(directory){
  setwd(directory)
  dir.create(paste(c(directory, "/static/"), collapse = ""))
  folders <- getFolders(directory)
  setupTemp(directory)
  setupStatic(directory, folders)
  setupGopher(directory)
}


#Put it All Together to Make Page
makePage <- function(directory, style,pageTitle,indexHeader) {
  print("Setting Up Site")
  setupSite(directory)
  print("Getting File Data")
  fileData <- makeFileData(directory)
  for(i in 1:dim(fileData)[1]){
    print("Setting up Page")
    print(fileData[i,]$title)
    makeHtml(fileData[i,],style = style,directory=directory)
  }
  print("Making Index")
  makeIndex(fileData,pageTitle)
  makeGophermap()
}

#Set it up. Make it go.
#directory = dirname(sys.frame(1)$ofile)
#setwd(directory)

# Local Building

directory = getwd()


style = paste(c(directory,"/style/style.css"),collapse="")
makePage(directory,style,"Greg Leo")

