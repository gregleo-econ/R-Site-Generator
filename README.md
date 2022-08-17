# R Site Generator

## How to use the generator. 

The markdown folder contains one one subfolder for each category of page you want on the index page of the site. Each subfolder must has one markdown file per page you want on the site. The markdown files should start with a  `+1` or `+2` or some other number. This is used for ordering pages (larger numbers are placed higher up) within a subgroup. Then there should be a single blank line and then a header `# Title`. This header will be used as the title of the page on the index.

The markdown folder should also contain a file called header.md which will be placed on top of every page (except the index). The markdown folder should also contain a file called indexHeader.md which will be placed at the top of the index. This can be whatever. Maybe a banner.

The global page stylesheet (css) file is in the /style subdirectory. Static files can be places in the /static directory.

To generate your site, run the pageMakerLinux.R code. 

## What's Supported?

```
# Header 1
## Header 2
### Header 3
*Italics*
**Bold**
`Inline Preformatted Text`

```
Code Blocks (Note these end in ''' instead of ```). 
'''

[Links](Google.com)
![Images](image.png)
![[Images as Links](image.png)](Google)
```