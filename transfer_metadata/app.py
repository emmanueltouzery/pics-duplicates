import flickr_api
import json

with open('picasa_albums.json') as f:
    picasaAlbums = json.load(f)

with open('picasa.json') as f:
    picasaData = json.load(f)

with open('flickr.json') as f:
    flickrSets = json.load(f)

flickr_api.set_keys("XXX", "XXXX")
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

print "%d picasa albums" % len(picasaAlbums)

albumIdx = 9

picasaTitle = picasaAlbums[albumIdx]["title"]
picasaId = picasaAlbums[albumIdx]["id"]
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

sets = user.getPhotosets()
set = filter(lambda x: x['id'] == pickedSet['info']['id'], sets)[0]
flickrPhotos = set.getPhotos()

picasaToFlickr = {}
videos = []
idx=0
for flickrPic in pickedSet['photos']['data']:
    title = flickrPic['title']
    picasaPicCandidates = filter(lambda x: x['filename'].lower() == (title + ".jpg").lower(), picasaPics)
    if len(picasaPicCandidates) != 1:
        allOptions = ", ".join(map(lambda x: x['filename'], picasaPics))
        print "Can't find picasa candidates for " + title + " all options: " + allOptions + " " + str(idx)
        # print flickrPic
        flickrPicObj = filter(lambda p: p.id == flickrPic["id"], flickrPhotos)[0]
        flickrPicInfo = flickrPicObj.getInfo()
        if flickrPicInfo["media"] == "video":
            print "it's a video!!"
            videos.append(flickrPic['id'])
    if flickrPic['id'] not in videos:
        picasaPic = picasaPicCandidates[0]
        picasaToFlickr[picasaPic['id']] = flickrPic['id']
    idx += 1

newPhotosOrder = map(lambda x: picasaToFlickr[x['id']], picasaPics)

print "Reordering pics..."
set.reorderPhotos(photo_ids = newPhotosOrder)
print "Reordered!"

picasaCover = picasaAlbums[albumIdx]['coverPhotoMediaItemId']
if picasaCover not in map(lambda x: x['id'], picasaPics):
    print "Cover picture is not in the list (probably a video), skipping it"
else:
    flickrCoverPhoto = picasaToFlickr[picasaCover]
    primaryPhoto = filter(lambda p: p.id == flickrCoverPhoto, flickrPhotos)[0]
    print "flickr cover photo: %s" % primaryPhoto
    set.setPrimaryPhoto(photo = primaryPhoto)

for picasaPic in filter(lambda p: 'description' in p, picasaPics):
    print picasaPic['id']
    photo = filter(lambda p: p.id == picasaToFlickr[picasaPic['id']], flickrPhotos)[0]
    print("Adding comment " + picasaPic['description'])
    print photo['title']
    photo.setMeta(title= photo['title'], description= picasaPic['description'])