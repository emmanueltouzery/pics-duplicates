import * as fs from "fs";
import * as crypto from "crypto";
import { HashMap, Vector, Future, Function2 } from "prelude-ts";

// Algorithm depends on availability of OpenSSL on platform
// Other algorithms: 'sha1', 'md5', 'sha256', 'sha512' ...
const hashAlgorithm = 'sha1';
const maxConcurrent = 4;

const folder = process.argv[2];

function getFilesAndSubfolders(baseFolder: string): HashMap<string,Vector<string>> {
    let result = HashMap.empty<string,Vector<string>>();
    const children = fs.readdirSync(baseFolder, {withFileTypes: true});
    const [folders, files] = Vector.ofIterable(children)
        .partition(d => d.isDirectory());
    result = result.put(baseFolder, files.map(f => f.name));
    return folders
        .map(f => baseFolder + "/" + f.name)
        .map(getFilesAndSubfolders)
        .foldLeft(result, (sofar,cur) => sofar.mergeWith(cur, (a,_)=>a));
}

function hashFile(filename: string): Future<string> {
    // https://gist.github.com/GuillermoPena/9233069
    const hasher = crypto.createHash(hashAlgorithm);

    const s = fs.createReadStream(filename)
    s.on('data', data => hasher.update(data));

    return Future.ofCallback(cb => s.on('end', cb))
        .map(_ => hasher.digest('hex'));
}

const filesAndSubfolders = getFilesAndSubfolders(folder);
console.log(`Found ${filesAndSubfolders.length()} folders containing ${
filesAndSubfolders.foldLeft(0, (sofar,cur)=>sofar+cur[1].length())} files`);

function hashFolder(folder: string, files: Vector<string>): Future<HashMap<string, string>> {
    const hashFileKeepPath = (f: string) => hashFile(folder+"/"+f)
        .map<[string,string]>(hash => [folder + "/" + f, hash]);
    return Future.traverse(files, hashFileKeepPath)
        .map(v => v.toMap(x=>x));
}

(async () => {

    const folderHashes = await Future.traverse(
        filesAndSubfolders,
        Function2.of(hashFolder).tupled(),
        {maxConcurrent});
    const fileToHash = folderHashes.foldLeft(
        HashMap.empty<string,string>(),
        (sofar,cur) => sofar.mergeWith(cur, (a,_)=>a));
    fileToHash.forEach(
        ([fname, hash]) => console.log(`${fname}\t${hash}`));
})();
