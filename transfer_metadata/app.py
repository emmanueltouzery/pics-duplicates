import flickr_api
import json

with open('picasa_albums.json') as f:
    picasaAlbums = json.load(f)

with open('picasa.json') as f:
    picasaData = json.load(f)

with open('flickr.json') as f:
    flickrSets = json.load(f)

flickr_api.set_keys("xxx", "xxx")
flickr_api.set_auth_handler("my_access.txt")
user = flickr_api.test.login()

#sets = user.getPhotosets()
#print "Found %d flickr albums" % (len(sets))
#
# photos = sets[0].getPhotos()
# print photos

# print photos[0].title

# for curSet in sets:
#  print json.dumps({ 'set': curSet, 'photos': curSet.getPhotos(), 'info': curSet.getInfo()}, default=lambda x: x.__dict__)

picasaTitle = picasaAlbums[0]["title"]
picasaId = picasaAlbums[0]["id"]
print "Dealing with picasa album %s [%s]" % (picasaTitle, picasaId)
picasaContents = filter(lambda x: x['albumId'] ==  picasaId, picasaData)[0]
picasaPics = picasaContents['data']['photos']
print "Found %d pictures" % (len(picasaPics))

flickrCandidates = filter(lambda x: x['info']['count_photos'] == len(picasaPics), flickrSets)
i = 0
def print_set(flickrSet):
    global i
    i += 1
    return str(i) + ". " + flickrSet['info']['title']

picasaFilenames = map(lambda x: x['filename'], picasaPics)
print picasaFilenames

print "Found %d flickr album candidates: \n%s" % (len(flickrCandidates), "\n".join(map(print_set, flickrCandidates)))
picked = input("Pick the album: ")
pickedSet = flickrCandidates[int(picked)-1]
print "Picked %s" % (pickedSet['info']['title'])

picasaToFlickr = {}
for flickrPic in pickedSet['photos']['data']:
    title = flickrPic['title']
    picasaPic = filter(lambda x: x['filename'] == title + ".jpg", picasaPics)[0]
    picasaToFlickr[picasaPic['id']] = flickrPic['id']

newPhotosOrder = map(lambda x: picasaToFlickr[x['id']], picasaPics)

sets = user.getPhotosets()
set = filter(lambda x: x['id'] == pickedSet['info']['id'], sets)[0]
# print "Reordering pics..."
# set.reorderPhotos(photo_ids = newPhotosOrder)
# print "Reordered!"

flickrPhotos = set.getPhotos()

flickrCoverPhoto = picasaToFlickr[picasaAlbums[0]['coverPhotoMediaItemId']]
primaryPhoto = filter(lambda p: p.id == flickrCoverPhoto, flickrPhotos)[0]
print "flickr cover photo: %s" % primaryPhoto
set.setPrimaryPhoto(photo = primaryPhoto)

for picasaPic in filter(lambda p: 'description' in p, picasaPics):
    print picasaPic['id']
    photo = filter(lambda p: p.id == picasaToFlickr[picasaPic['id']], flickrPhotos)[0]
    print("Adding comment " + picasaPic['description'])
    print photo['title']
    photo.setMeta(title= photo['title'], description= picasaPic['description'])
