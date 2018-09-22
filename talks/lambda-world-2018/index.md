---
title: Radix Trees—How IntMap Works
author: Tikhon Jelvis
---

<div class="content">

# Radix Trees

I gave a talk at [Lambda World 2018][lambda-world] about the radix tree, the data structure behind Haskell's `Data.IntMap` as well as recently published variant on the same structure called the *adaptive* radix tree.

  * [slides](slides.html)
  
  [lambda-world]: https://seattle.lambda.world

## References

  * Chris Okasaki and Andy Gill, "Fast Mergeable Integer Maps", *Workshop on ML*, September 1998, pages 77-86 ([PDF][okasaki])
  
  * V. Leis, A. Kemper and T. Neumann. The adaptive radix tree: ARTful indexing for main-memory databases. Proceedings of the 2013 IEEE International Conference on Data Engineering ([PDF][art])
  
  [okasaki]: https://www.westpoint.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/ml98maps.pdf
  [art]: https://db.in.tum.de/~leis/papers/ART.pdf

</div>
