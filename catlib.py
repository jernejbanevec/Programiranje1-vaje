import requests
import re
import os
import csv


########################################################################
# First, let's write some functions to get the data from the web.
########################################################################

# define the URL of the main page of the bolha cats listing
cats_frontpage_url = "http://www.bolha.com/zivali/male-zivali/macke/"
# the directory to which we save our data
cat_directory = "Macke_podatki"
# the filename we use to save the frontpage
frontpage_filename = "macke_neurejene"
# the filename for the CSV file for the extracted data
csv_filename = "macke_urejene"

def download_url_to_string(url):
    '''This function takes a URL as argument and tries to download it
    using requests. Upon success, it returns the page contents as string.'''
    webpage = requests.get(url)
    if webpage.ok:
        #no error
        webpage_text = webpage.text
    else:
        #error
        webpage_text = "Unable to get webpage."
    return webpage_text


def save_string_to_file(text, directory, filename):
    '''Write "text" to the file "filename" located in directory "directory",
    creating "directory" if necessary. If "directory" is the empty string, use
    the current directory.'''
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as file_out:
        file_out.write(text)
    return None


# Define a function that downloads the frontpage and saves it to a file.
def save_cat_webpage():
    webpage_text = download_url_to_string(cats_frontpage_url)
    save_string_to_file(webpage_text, cat_directory, frontpage_filename)
    return None

#save_cat_webpage()



########################################################################
# Now that we have some data, we can think about processing it.
########################################################################

def read_file_to_string(directory, filename):
    '''Return the contents of the file "directory"/"filename" as a string.
    '''
    path = os.path.join( directory, filename )
    with open(path, 'r') as file_in:
        return file_in.read()


# Define a function that takes a webpage as a string and splits it into
# segments such that each segment corresponds to one advertisement. This
# function will use a regular expression that delimits the beginning and end of
# each ad. Return the list of strings.
# Hint: To build this reg-ex, you can use your text editor's regex search functionality.
def split_text_to_ads(text):
    ad_pattern = re.compile("<h3><a title=(.*?)miscellaneous", re.DOTALL)
    #return ad_pattern.findall(text)
    return [x.group(1) for x in ad_pattern.finditer(text)]


# Define a function that takes a string corresponding to the block of one
# advertisement and extracts from it the following data: Name, price, and
# the description as displayed on the page.
def get_data_from_ad( ad_text ):
    ad_data_pattern = re.compile(r"(?P<Name>.*?) href"
                                 r".*?</h3>(?P<Description>.*?)<div"
                                 r".*?<div class=\"price\">(?P<Price>.*?)</div>"
                                 ,re.DOTALL)
    ad_match = ad_data_pattern.search(ad_text)
    return ad_match.groupdict()
    



# Write a function that reads a page from a file and returns the list of
# dictionaries containing the information for each ad on that page.
def get_ad_from_file(directory,filename):
    text = read_file_to_string(directory, filename)
    ads = split_text_to_ads(text)
    data = [get_data_from_ad(ad) for ad in ads]
    return data
    



########################################################################
# We processed the data, now let's save it for later.
########################################################################

def write_csv(fieldnames, rows, directory, filename):
    '''Write a CSV file to directory/filename. The fieldnames must be a list of
    strings, the rows a list of dictionaries each mapping a fieldname to a
    cell-value.

    '''
    os.makedir(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w') as csv_file:
        writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)
    return None


# Write a function that takes a list of cat advertisement dictionaries and
# writes it to a csv file.
def undefined( TODO ):
    '''TODO'''
    TODO
