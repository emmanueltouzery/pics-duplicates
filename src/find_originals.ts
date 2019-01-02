import * as fs from "fs";
import { Vector, HashMap } from "prelude-ts";
import { spawnSync } from "child_process";

const [picasaFolder, allOldPicsFile] = process.argv.slice(2);

const picasaPics = fs.readdirSync(picasaFolder);

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

function getFileName(fname: string) {
    if (fname === "") {
        return ""
    }
    return Vector.ofIterable(fname).takeRightWhile(c => c != '/').mkString("");
}

const allOldPicFiles = Vector.ofIterable(readHashes(allOldPicsFile).valueIterable())
    .filter(fname => fname.indexOf("small") < 0)
    .groupBy(fname => getFileName(fname));


const targetFolder = getFileName(picasaFolder);
if (fs.existsSync(targetFolder)) {
    console.error(`target folder ${targetFolder} already exists, aborting`);
    process.exit(1);
}
fs.mkdirSync(targetFolder);

for (const picasaPic of picasaPics) {
    const oldFile = picasaFolder + "/" + picasaPic;
    let fileToUse = oldFile;
    if (!allOldPicFiles.containsKey(picasaPic)) {
        console.warn(`Can't find a full-res image for ${picasaPic}`);
    } else {
        let newFiles = allOldPicFiles.get(picasaPic).getOrThrow();
        if (newFiles.length() > 1) {
            const result = spawnSync("compare-pics", newFiles.toArray());
            console.log(`user picked ${result.stdout.toString()}`)
            newFiles = Vector.of(result.stdout.toString().trim());
        }
        const newFile = newFiles.head().getOrThrow();
        const oldSize = fs.statSync(oldFile).size;
        const newSize = fs.statSync(newFile).size;
        if (newSize > oldSize) {
            fileToUse = newFile;
        } else {
            console.warn(`New file is smaller than original for ${picasaPic}`)
            console.log(oldFile);
            console.log(newFile);
        }
    }
    fs.copyFileSync(fileToUse, targetFolder + "/" + picasaPic);
}
