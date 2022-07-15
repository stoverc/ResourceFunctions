# Resource Functions

This is a collection of functions I either have submitted or will soon submit to the [Wolfram Function Repository](https://resources.wolframcloud.com/FunctionRepository/).

My ```$PublisherID``` is [TheRealCStover](https://resources.wolframcloud.com/publishers/resources?PublisherID=TheRealCStover).

## Summary
Here is a snapshot:

| Resource | Versions in this Repo | Published Version |
| ----------- | ----------- | ----------- |
| [PlayWordle](https://resources.wolframcloud.com/FunctionRepository/resources/PlayWordle/) | v1.1.0 (22 Jun 2022) <br> v1.0.0 (20 June 2022) <br> v0.1.0 (14 Jun 2022)) | v1.1.0 |
| [RandomMetalPseudoSubgenre](https://resources.wolframcloud.com/FunctionRepository/resources/RandomMetalPseudoSubgenre/) | v1.0.0 (20 Jun 2022) | v1.0.0 |
| [InactiveSumOfPowers](https://resources.wolframcloud.com/FunctionRepository/resources/InactiveSumOfPowers/) | v1.0.0 (15 Jul 2022) <br> v0.3.0 (3 Jul 2022) <br> v0.2.0 (20 Jun 2022) <br> v0.1.0 (20 Jun 2022) | (coming soon) |
| BenfordAnalysis<br><sub>(no documentation yet)</sub> | v0.1.0 (2 Jul 2022) | (coming soon) |

## Changelog
<h3>15 Jul 2022</h3>
<ol>
  <li>Uploaded <i>actual</i> submission-quality version of <code>InactiveSumOfPowers</code> (nee <code>SumOfIntegerPowers</code>) and renamed the other files in the directory to reflect the current name, versioning, etc.</li>
</ol>

<details>
  <summary><h2>Older Changes</h2></summary>
  <details>
  <h3>3 Jul 2022</h3>
  <ol>
    <li>Uploaded submission-quality versions of <code>SumOfIntegerPowers</code> files (<code>.wl</code> and <code>.nb</code>). Also, submitted said function to the WFR.</li>
  </ol>
  <summary><h3>2 Jul 2022</h3></summary>
  <h4>BenfordAnalysis</h4>
  <ol>
    <li>Created my first branch, and uploaded a preliminary (0.1.0) version of <code>BenfordAnalysis</code>.</li>
    <li>Later, updated the <code>README.md</code> file to tidy up the change log a bit.</li>
  </ol>
  </details>
  <details>
  <summary><h3>20 Jun 2022</h3></summary>
    <h4>SumOfIntegerPowers</h4>
    <ol>
      <li>Initial upload.</li>
      <li>Linked table entry to temporary cloud-deployed documentation.</li>
      <li>Later, updated README to reflect above-mentioned linking.</li>
      <li>Much later, made considerable code updates. Apparently, this thing was buggy, and those bugs had slipped through the cracks during my original publication without me realizing. :\</li>
    </ol>
    <h4>RandomMetalPseudoSubgenre</h4>
    <ol>
      <li>Initial upload.</li>
      </ol>
      <h4>PlayWordle</h4>
      <ol>
      <li>I deleted all the old files in the repo and renamed them according to the standard naming in WFR def notebooks.</li>
      <li>I added v1.1.0 as both a .wl and a .nb file.</li>
      <li>Later, I realized that "v1.0.0" was actually v0.1.0, so I fixed the versioning issue on my end + reuploaded everything. This means that there are <i>three</i> versions now.</li>
      <li>Later still, I found out a better way to generate .wl files than <code>Export[...]</code>, so I implemented that + configured some of the file names for consistency.</li>
    </ol>
  </details>
  <details>
  <summary><h3>14 Jun 2022</h3></summary>
    <h4>PlayWordle</h4>
    <ol>
      <li>The first few commits here have been (and will continue to be) older-than-published versions, just for the sake of getting this repo caught up with the current status of the files as they exist in the WFR. Once this is done, commits will happen in a way that promotes CI/CD in the usual sense.</li>
      <li>Later, I added a copy of the published author notes to the existing WL file. This is a temporary solution, and eventually, I'm going to change my directory structure(s) to better reflect the standard GitHub implementation of WFR function directories.</li>
    </ol>
  </details>
</details>
