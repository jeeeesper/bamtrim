# BAM Trim

This simple Haskell program trims SAM-formatted reads to only a specified region and ouputs them as FASTA.
The tool was written for prototyping downstream tools, like sequence aligners or variation visualisers.

## Compilation
Requires a Haskell compiler. The command
```
  ghc bamtrim.hs
```
will compile the source code to an executable file.

## Usage
The program expects SAM-formatted input strings on standard input and one argument, a region specifier like `chr1:123-456`.
For example, the command
```
  samtools view aligned_and_sorted_reads.bam chr1:123-456 | ./bamtrim chr1:123-456
```
first filters for reads that (partially) overlap the specified position with `samtools`, and bamtrim then truncates the output reads to only include the specified region.

## Limitations
This is a very quickly hacked-together script that will likely fail in some edge cases. Handle with care.
