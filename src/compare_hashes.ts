import * as fs from "fs";
import { HashMap, Vector } from "prelude-ts";

const [file1, file2] = process.argv.slice(2);

function readHashes(filename: string): HashMap<string, string> {
    let lines = fs.readFileSync(filename).toString().split("\n");
    if (lines[0].startsWith("Found ")) {
        lines = lines.slice(1);
    }
    return HashMap.ofIterable(lines.map<[string,string]>(l => {
        const [path, hash] = l.split("\t");
        return [hash, path];
    }));
}

const hashes1 = readHashes(file1);
console.log(`${hashes1.length()} files after hash-unification in the first file`);
const hashes2 = readHashes(file2);
console.log(`${hashes2.length()} files after hash-unification in the second file`);

const onlyInFile2 = hashes2.filterKeys(hash2 => !hashes1.containsKey(hash2));

console.log(`${onlyInFile2.length()} files only in the second list`);

const foldersOnlyInFile2 = Vector.ofIterable(onlyInFile2.valueIterable())
    .toSet(path => Vector.ofIterable(path).dropRightWhile(c => c != '/').mkString(""))
    .filter(f => !f.endsWith("/small/"));
console.log(`${foldersOnlyInFile2.length()} folders with files only in the second list`);
console.log(foldersOnlyInFile2.mkString("\n"))
