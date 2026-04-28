<TeXmacs|2.1>

<style|generic>

<\body>
  <\algorithm>
    A numbered algorithm.

    Step 1

    Step 2
  </algorithm>

  <\algorithm*>
    An unnumbered algorithm.

    Step 1

    Step 2
  </algorithm*>

  <\named-algorithm|Gradient Descent>
    Initialize <math|x>

    Update <math|x>
  </named-algorithm>

  <\specified-algorithm>
    A specified and numbered algorithm.
  <|specified-algorithm>
    Step 1

    Step 2
  </specified-algorithm>

  <\specified-algorithm*>
    A specified and unnumbered algorithm.
  <|specified-algorithm*>
    Step 1

    Step 2
  </specified-algorithm*>

  <\algorithm>
    <algo-require|sorted array <math|A>, target <math|t>>

    <algo-ensure|index of <math|t> in <math|A> or <math|-1>>

    <\algo-if-else-if>
      <\algo-if|<math|n=0>>
        <algo-return|<math|-1>>
      </algo-if>

      <\algo-else-if|<math|A<around|[|mid|]>=t>>
        <algo-return|<math|mid>>
      </algo-else-if>

      <\algo-else>
        <algo-return|<algo-call|BinarySearch|<math|A,t>>>
      </algo-else>
    </algo-if-else-if>
  </algorithm>

  <\algorithm>
    <\algo-for|<math|i=1> <algo-to> <math|n>>
      <\algo-while|<math|A<around|[|i|]>\<gtr\>0>>
        <algo-state|<math|A<around|[|i|]>\<leftarrow\>A<around|[|i|]>-1>>
      </algo-while>
    </algo-for>
  </algorithm>
</body>
</TeXmacs>
