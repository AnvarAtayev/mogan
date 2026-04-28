---
math: true
---
{{% algorithm number="1" %}}
A numbered algorithm.

Step 1

Step 2
{{% /algorithm %}}

{{% algorithm %}}
An unnumbered algorithm.

Step 1

Step 2
{{% /algorithm %}}

{{% algorithm number="2" title="Gradient Descent" %}}
Initialize $x$

Update $x$
{{% /algorithm %}}

{{% algorithm number="3" %}}
A specified and numbered algorithm.

Step 1

Step 2
{{% /algorithm %}}

{{% algorithm %}}
A specified and unnumbered algorithm.

Step 1

Step 2
{{% /algorithm %}}

{{% algorithm number="4" %}}
**Require:** sorted array $A$, target $t$

**Ensure:** index of $t$ in $A$ or $-1$

**if** $n=0$ **then**
{{% indent %}}
**return** $-1$
{{% /indent %}}
**else if** $A[mid]=t$ **then**
{{% indent %}}
**return** $mid$
{{% /indent %}}
**else**
{{% indent %}}
**return** BinarySearch($A,t$)
{{% /indent %}}
**end if**
{{% /algorithm %}}

{{% algorithm number="5" %}}
**for** $i=1$ **to** $n$ **do**
{{% indent %}}
**while** $A[i]>0$ **do**
{{% indent %}}
$A[i]\leftarrow A[i]-1$
{{% /indent %}}
**end while**
{{% /indent %}}
**end for**
{{% /algorithm %}}
