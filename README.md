# ResourceFunctions

This is a collection of functions I either have submitted or will soon submit to the [Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository/).

My ```$PublisherID``` is [TheRealCStover](https://resources.wolframcloud.com/publishers/resources?PublisherID=TheRealCStover).

## Summary
Here is some data:

| Resource | Versions in this Repo | Published Version |
| ----------- | ----------- | ----------- |
| [PlayWordle](https://resources.wolframcloud.com/FunctionRepository/resources/PlayWordle/) | v1.1.0 (22 Jun 2022) <br> v1.0.0 (20 June 2022) <br> v0.1.0 (14 Jun 2022)) | v1.1.0 |
| [RandomMetalPseudoSubgenre](https://resources.wolframcloud.com/FunctionRepository/resources/RandomMetalPseudoSubgenre/) | v1.0.0 (20 Jun 2022) | v1.0.0 |
| [SumOfIntegerPowers](https://www.wolframcloud.com/obj/cstover/DeployedResources/Function/SumOfIntegerPowers/)<br><sub>(temporary cloud documentation)</sub> | v0.2.0 (20 Jun 2022) <br> v0.1.0 (20 Jun 2022) | (coming soon) |

## Changelog
### 20 Jun 2022
#### SumOfIntegerPowers
1. Initial upload.
2. Linked table entry to temporary cloud-deployed documentation.
3. Later, updated README to reflect above-mentioned linking.
4. Much later, made considerable code updates. Apparently, this thing was buggy, and those bugs had slipped through the cracks during my original publication without me realizing. :\
#### RandomMetalPseudoSubgenre
1. Initial upload.
#### PlayWordle
1. I deleted all the old files in the repo and renamed them according to the standard naming in WFR def notebooks.
2. I added v1.1.0 as both a .wl and a .nb file.
3. Later, I realized that "v1.0.0" was actually v0.1.0, so I fixed the versioning issue on my end + reuploaded everything. This means that there are _three_ versions now.
4. Later still, I found out a better way to generate .wl files than `Export[...]`, so I implemented that + configured some of the file names for consistency.
### 14 Jun 2022
#### PlayWordle
1. The first few commits here have been (and will continue to be) older-than-published versions, just for the sake of getting this repo caught up with the current status of the files as they exist in the WFR. Once this is done, commits will happen in a way that promotes CI/CD in the usual sense.
2. Later, I added a copy of the published author notes to the existing WL file. This is a temporary solution, and eventually, I'm going to change my directory structure(s) to better reflect the standard GitHub implementation of WFR function directories.
