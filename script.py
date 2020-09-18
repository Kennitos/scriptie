# imports
import requests
import re
import os
import json

# from bs4 import BeautifulSoup
import bs4
from git import Repo
# import git    # add 'git' module through anaconda navigator + pip install gitpython

def try_link_getter(ul_list):
    """
    Explanation function
    """
    links = []
    for ul in ul_list:
        for line in ul:
            try:
                links.append(line.a.get('href'))
            except:
                pass
    return links

def download_(url):
    """
    Explanation function
    """
    name = url.split('/')[-1]
    r  = requests.get(url)
    data = r.text
    soup = bs4.BeautifulSoup(data,features="html.parser")
    convert = soup.get_text()
    try: 
        json_data = json.loads(convert)
        #json_data = json.loads(convert.replace(" ","").replace("\n",""))
        cwd = os.getcwd()
        with open(cwd+'\\single_files\\'+name, "w") as outfile:
            json.dump(json_data,outfile)
    except Exception as e:
    	pass
#       	print(e,url)
#         if 'Extra data' in e:
#             print("404 Page not found: ",url)
# #         else:
#             print("Downloading failed for url: ",url)


def nbviewer_to_download(nbviewer):
    """
    Explantion function
    """
    try:
        m = re.search(".org/github", nbviewer)
        if m != None:
            prefix = 'https://raw.githubusercontent.com'
            download_url = prefix+nbviewer[m.end():].replace('/blob','')
            download_(download_url)
        elif re.search(".org/url/", nbviewer) != None:
            m = re.search(".org/url", nbviewer)
            download_url = 'http://'+nbviewer[m.end()+1:]
            download_(download_url)
    except Exception as e:
    	pass
        # print(e)
        # print(nbviewer)


def clone_repos(repo_name,url,folder):
    """
    Explenation function
    """
    cwd = os.getcwd()
    try:
        repo_dir = cwd+'\\'+folder+'\\'+repo_name
        correct_url = '/'.join(url.split('/')[:5]) # makes sure it is a url to a repository (not blob/master/readme etc.) 
        Repo.clone_from(correct_url,repo_dir)
    except Exception as e:
    	pass
        # print(e)
        # print('Failed for:',repo_name,url)



### Webscraping <i>A gallery of interesting Jupyter Notebooks</i>

def scrape_page(url):
    print("Scraping links from page, takes approx 1 min")
    r  = requests.get(url)
    data = r.text
    soup = bs4.BeautifulSoup(data,features="html.parser")

    body = soup.find_all("div", {"class": "markdown-body"})[0] # using [0] at the end since there is only 1 result of a div with class 'markdown-body'
    ul_list = body.find_all('ul')

    links = try_link_getter(ul_list)
    print("Complete")
    github_repos = [link for link in links if 'nbviewer' not in link and 'github.com' in link]
    github_single_files = [url for url in links if 'nbviewer' in url and 'github' in url]

    print("Downloading single files, takes approx 3 min")
    if not os.path.exists('single_files'):
        os.makedirs('single_files')
    for url in github_single_files:
        nbviewer_to_download(url)
    print("Complete")

    print("Cloning repositories, takes approx 3 min")
    if not os.path.exists('repos'):
        os.makedirs('repos')
    for url in github_repos:
        try:
            folder_name = url.split('/')[4]
            clone_repos(folder_name,url,'repos')
        except:
            pass
    #        print(url) # one url is not a link to a repository but to the user's page
    print("Complete")

    duplicates_repo_url = [repo for repo in github_repos if github_repos.count(repo)>1]
    print(duplicates_repo_url)



url = "https://github.com/jupyter/jupyter/wiki/A-gallery-of-interesting-Jupyter-Notebooks"
scrape_page(url)

# r  = requests.get(url)
# data = r.text
# soup = bs4.BeautifulSoup(data,features="html.parser")

# body = soup.find_all("div", {"class": "markdown-body"})[0] # using [0] at the end since there is only 1 result of a div with class 'markdown-body'
# ul_list = body.find_all('ul')

# links = try_link_getter(ul_list)
# github_repos = [link for link in links if 'nbviewer' not in link and 'github.com' in link]
# github_single_files = [url for url in links if 'nbviewer' in url and 'github' in url]

# if not os.path.exists('single_files'):
#     os.makedirs('single_files')
# for url in github_single_files:
#     nbviewer_to_download(url)

# if not os.path.exists('repos'):
#     os.makedirs('repos')
# for url in github_repos:
#     try:
#         folder_name = url.split('/')[4]
#         clone_repos(folder_name,url,'repos')
#     except:
#         pass
#        print(url) # one url is not a link to a repository but to the user's page





# uitleggen waarom nbviewer.gist exclude from list!!
# nbviewer_list = [link for link in links if 'nbviewer' in link]

# print("Downloaded all links from page")


# ### Downloading single files from url's
# # create folder in which to put all the files (if it does not exist)
# if not os.path.exists('single_files'):
#     os.makedirs('single_files')

# print(1,os.getcwd())

# # ALL SINGLE GITHUB FILES (NBVIEWER)
# for url in github_single_files:
#     nbviewer_to_download(url)
    

# print("Downloaded single files")

# ### Clone repositories from url's
# # create folder in which to put all the repositories (if it does not exist)
# if not os.path.exists('repos'):
#     os.makedirs('repos')

# # ALL GITHUB REPOS
# for url in github_repos
#     try:
#         folder_name = url.split('/')[4]
#         clone_repos(folder_name,url,'repos')
#     except:
#         pass
# #        print(url) # one url is not a link to a repository but to the user's page

# duplicates_repo_url = [repo for repo in github_repos if github_repos.count(repo)>1]
# # duplicates_repo_url

# print("Cloned all repositories from page")

def count_ipynb(folder):
    path = os.getcwd()+'\\'+folder
    try:
        dir_list = os.listdir(path)
    except:
        return "this repository doesn't exist"

    file_id = 0
    for root, dirs, files in os.walk(path):
#         for file in tqdm_notebook(files):
        for file in files:
            if file.endswith(".ipynb"):
                file_id += 1
    return file_id

count_ipynb('single_files'),count_ipynb('repos'),count_ipynb('notebooks'),count_ipynb('fake_folder')

