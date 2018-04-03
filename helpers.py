import string
import re

def ellipsis(s):
    return (re.match(r'.*\.{3,}.*', s) != None)

def shriek(s):
    return (re.match(r'.*\!{1,}.*', s) != None)

def waah(s):
    return (re.match(r'.*\?{2,}.*', s) != None) or (re.match(r'.*\!\?{1,}.*', s) != None)

def url(s):
    return (re.match(r'\bhttp(s?)\:\/{1,2}.*\b', s) != None)

def ip(s):
    return (re.match(r'.*\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}.*', s) != None) and not url(s)

def email(s):
    return (re.match(r'\b[a-zA-Z0-9\.\_\-]+@[a-zA-Z0-9\.\_\-]+\b', s) != None) and not url(s)
                                           
def allcaps(s):
    return (re.match(r'\b[A-Z]{3,}\b', s) != None)

def num(s):
    return (re.match(r'\d+', s) != None)

def wiki(s):
    return (re.match(r'Wikipedia\:*', s) != None)

def image(s):
    return (re.match(r'.+Image\:*', s) != None)

# NOT WORKING YET
def unic(s): 
    #return (re.match(r'.*(\\x[a-z0-9]{2})+.*', s) != None)
    return (re.match(r'\b\\xd8.*', s) != None)


def normalise(s):
    words = s.split()
    tmp = []
    for w in words:
        shk = shriek(w)
        wah = waah(w)            
        if(allcaps(w)): tmp += ['__CAPS__']
        if(email(w)): tmp += ['__EMAIL__']
        elif(url(w)): tmp += ['__URL__']
        elif(ip(w)): tmp += ['__IP__']
        elif(wiki(w)): tmp += ['__WIKI__']
        elif(image(w)): tmp += ['__IMG__']
        else: 
            w = w.replace('/',' ')
            w = w.replace('-',' ')
            w = w.replace('_',' ')
            w_tmp = w.split()
            for x in w_tmp:
                ww = x.lower().translate(None, string.punctuation)
                if(num(ww)): ww = '__NUM__'
                tmp += [ww]
        if shk:
            tmp += ['__!!!__']
        if wah:
            tmp += ['__???__']
    return ' '.join(tmp)