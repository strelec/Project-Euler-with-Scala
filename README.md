Short and concise Scala solutions to Project Euler problems.
===

This is a repository with my Scala solutions to Project Euler problems.

I was morally divided whether to publish them or not, since I don't want to spoil the fun for others. But because more than a dozen people have asked me about the Scala solutions or were unable to produce solutions below few hundred lines, I have decided to publish them anyways. And also for the benefit of the Scala programming language. But beware, solutions ahead.

The files with E in the filename contain explanation in the block comment at the beginning of the file.

Rules
---

While coding, I abide by the few simple rules:

1. No "allnighters". The script has to produce **a result in 5 seconds at the worst**. In practice, it does even better, in a few millisecond range.
2. Avoid code duplication. If some functionality is needed multiple times, extract it into the `Helpers.scala` library / file.
3. Prune the search space as much as possible. If the task requires you to output the biggest result, make an iterator that generates results in order and just call `.next` on the resulting iterator. This way we avoid much of the unneeded calculation.
