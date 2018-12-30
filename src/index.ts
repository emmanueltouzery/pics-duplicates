import * as fs from "fs";
import { HashMap, Vector } from "prelude-ts";

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

const filesAndSubfolders = getFilesAndSubfolders(folder);
console.log(`Found ${filesAndSubfolders.length()} folders containing ${
    filesAndSubfolders.foldLeft(0, (sofar,cur)=>sofar+cur[1].length())} files`);
