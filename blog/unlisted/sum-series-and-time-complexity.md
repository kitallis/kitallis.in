---
title: sum series and time complexity
date: 2026-05-24
---

My highly sophisticated rule of thumb for eyeballing time complexity is to find the cheapest unit of work and count how many times it runs. I find it a bit more reliable than _counting the nested loops_. This is easy to see why:

```ruby
left = 0
(0...n).each do |right|
  while terrible_condition
    left += 1
  end
end
```

Here, `right` and `left` only move forward **once**, so the total amount of movement is `2n`, linear time.

Similarly, a nested loop that doubles against the cut off internally:

```ruby
(1...n).each do |i|
  j = i
  while j < n
    j *= 2
  end
end
```

The outer loop runs `n` times (from `1` to `n-1`) and the inner loop runs _possibly_ `n` times. But not really, the inner loop `j` doubles with respect to `i`. The number of times it doubles goes like this:

| `i` | → `2i` | → `4i` | → `8i` | → `n` |
|-----|--------|--------|--------|-------|

Repeated doubling (or halving) that reaches `n` is logarithmic, so you get an inequality like this:

<math display="inline">
  <msup><mn>2</mn><mi>k</mi></msup>
  <mo>×</mo>
  <mi>i</mi>
  <mo>&lt;</mo>
  <mi>n</mi>
</math>

<math display="inline">
  <msup><mn>2</mn><mi>k</mi></msup>
  <mo>&lt;</mo>
  <mfrac><mi>n</mi><mi>i</mi></mfrac>
</math>

<math display="inline">
  <mi>k</mi>
  <mo>&lt;</mo>
  <mi>log</mi>
  <mrow>
    <mo>(</mo>
    <mfrac><mi>n</mi><mi>i</mi></mfrac>
    <mo>)</mo>
  </mrow>
</math>

So for every `i`, <math display="inline"><mi>log</mi><mrow><mo>(</mo><mfrac><mi>n</mi><mi>i</mi></mfrac><mo>)</mo></mrow></math> is the number of operations. For all `i`, it's <math display="inline"><mo>∑</mo><mi>log</mi><mrow><mo>(</mo><mfrac><mi>n</mi><mi>i</mi></mfrac><mo>)</mo></mrow></math>, since we're just counting the operations (k), not the actual value (that's what the program is for!)

<math display="inline">
  <munderover>
    <mo>∑</mo>
    <mrow><mi>i</mi><mo>=</mo><mn>1</mn></mrow>
    <mi>n</mi>
  </munderover>
  <mi>log</mi>
  <mrow>
    <mo>(</mo>
    <mfrac><mi>n</mi><mi>i</mi></mfrac>
    <mo>)</mo>
  </mrow>
  <mo>=</mo>
  <mi>n</mi><mo>·</mo><mi>log</mi><mo>(</mo><mi>n</mi><mo>)</mo>
  <mo>−</mo>
  <mi>log</mi><mo>(</mo><mi>n</mi><mo>!</mo><mo>)</mo>
  <mo>≈</mo>
  <mi>n</mi>
</math>

<math display="inline"><mo>∑</mo><mi>log</mi><mrow><mo>(</mo><mfrac><mi>n</mi><mi>i</mi></mfrac><mo>)</mo></mrow></math> reduces down to <math display="inline"><mi>O</mi><mo>(</mo><mi>n</mi><mo>)</mo></math> after applying [Stirling's approximation](https://en.wikipedia.org/wiki/Stirling%27s_approximation). The derivation of how that happens is less interesting than the fact that knowing your series summations can be pretty useful in intuiting how time complexity analyses (tricky ones especially) work.

In both examples, it might _appear_ (if poorly analysed) that these algorithms are quadratic, but they aren't.

<hr>

This got me attempting a rather tricky one, even though I tacitly know this is something like <math display="inline"><mi>O</mi><mo>(</mo><mi>log</mi><mi>log</mi><mi>n</mi><mo>)</mo></math>, I was having a hard time understanding _why_:

```ruby
i = n
while i > 2
  i = Math.sqrt(i)
end
```

After some dubious scratch math, I managed to arrive at a no-prior-inspiration analysis. The only _prior_ is the definition of logarithm itself <math display="inline"><msup><mn>2</mn><mi>x</mi></msup><mo>=</mo><mi>n</mi></math>, x = <math display="inline"><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>n</mi><mo>)</mo></math>. Or more generally, <math display="inline"><msup><mi>B</mi><mi>x</mi></msup><mo>=</mo><mi>n</mi></math>, x = <math display="inline"><msub><mi>log</mi><mi>B</mi></msub><mo>(</mo><mi>n</mi><mo>)</mo></math>, where B is the base.

In the code above, we start with `n` and repeatedly take square-roots. Square roots are simply fractional powers, <math display="inline"><msup><mi>n</mi><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math>.

The loop runs until we hit just over `2`. Let's say it runs k times (since we don't really know yet, by just looking at it). It goes like this, for n=65536, it'd be k=4:

| iteration | value | is also |
|---|---|---|
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>16</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math> | 256 | → <math display="inline"><msup><mn>2</mn><mn>8</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>8</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math> | 16 | → <math display="inline"><msup><mn>2</mn><mn>4</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>4</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math> | 4 | → <math display="inline"><msup><mn>2</mn><mn>2</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>2</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math> | 2 | → <math display="inline"><msup><mn>2</mn><mn>1</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>1</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math> | 1 | → &lt;stop&gt; |

In other words, we're doing,

| iteration | value | is also |
|---|---|---|
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>16</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>2</mn></mfrac></msup></math> | 256 | → <math display="inline"><msup><mn>2</mn><mn>8</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>16</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>4</mn></mfrac></msup></math> | 16 | → <math display="inline"><msup><mn>2</mn><mn>4</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>16</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>8</mn></mfrac></msup></math> | 4 | → <math display="inline"><msup><mn>2</mn><mn>2</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>16</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>16</mn></mfrac></msup></math> | 2 | → <math display="inline"><msup><mn>2</mn><mn>1</mn></msup></math> |
| <math display="inline"><msup><mrow><mo>(</mo><msup><mn>2</mn><mn>16</mn></msup><mo>)</mo></mrow><mfrac><mn>1</mn><mn>32</mn></mfrac></msup></math> | 1 | → &lt;stop&gt; |

This is just, <math display="inline"><msup><mi>(n)</mi><mfrac><mn>1</mn><msup><mn>2</mn><mi>k</mi></msup></mfrac></msup></math> where, n=<math display="inline"><msup><mn>2</mn><mn>16</mn></msup></math> (our original starting number). We're reducing the fractional exponent k times to arrive roughly above `2`, our target cut-off point. The point of writing the reduction in the form of a fixed 2^16 is so we can treat the expression as `n`, because that's the input size that we'll measure the time complexity against.

It's also important to note that we're not interested in finding the scalar value of `k`, we're interested in knowing how much more time things take as `n` grows.

So it'd be fair to say that our loop represents this inequality:

<math display="inline">
  <msup>
    <mi>n</mi>
    <mfrac>
      <mn>1</mn>
      <msup><mn>2</mn><mi>k</mi></msup>
    </mfrac>
  </msup>
  <mo>></mo>
  <mn>cut_off</mn>
</math>

or, in powers of two (so we're working against same-base logs),

<math display="inline">
  <msup>
    <mrow>
      <mo>(</mo>
      <msup>
        <mn>2</mn>
        <mi>m</mi>
      </msup>
      <mo>)</mo>
    </mrow>
    <mfrac>
      <mn>1</mn>
      <msup>
        <mn>2</mn>
        <mi>k</mi>
      </msup>
    </mfrac>
  </msup>
  <mo>&gt;</mo>
  <mi>cut_off</mi>
</math>

We need to solve for `k`, but it's stuck as a non-trivial exponent on top, but since <math display="inline"><msup><mn>2</mn><mi>x</mi></msup><mo>=</mo><mi>n</mi></math>, x = <math display="inline"><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>n</mi><mo>)</mo></math>, we can use that to pull it down:

<math display="inline">
  <mfrac>
    <mi>m</mi>
    <msup><mn>2</mn><mi>k</mi></msup>
  </mfrac>
  <mo>&gt;</mo>
  <msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo>
</math>

<math display="inline">
  <mi>m</mi>
  <mo>&gt;</mo>
  <msup><mn>2</mn><mi>k</mi></msup>
  <mo>·</mo>
  <msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo>
</math>

<math display="inline">
  <mfrac>
    <mi>m</mi>
    <mrow><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo></mrow>
  </mfrac>
  <mo>&gt;</mo>
  <msup><mn>2</mn><mi>k</mi></msup>
</math>

<math display="inline">
  <msup><mn>2</mn><mi>k</mi></msup>
  <mo>&lt;</mo>
  <mfrac>
    <mi>m</mi>
    <mrow><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo></mrow>
  </mfrac>
</math>

<math display="inline">
  <mi>k</mi>
  <mo>&lt;</mo>
  <msub><mi>log</mi><mn>2</mn></msub>
  <mrow>
    <mo>(</mo>
    <mfrac>
      <mi>m</mi>
      <mrow><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo></mrow>
    </mfrac>
    <mo>)</mo>
  </mrow>
</math>

<i>since, <math display="inline"><msup><mn>2</mn><mi>m</mi></msup><mo>=</mo><mi>n</mi></math>, m = <math display="inline"><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>n</mi><mo>)</mo></math></i>

<math display="inline">
  <mi>k</mi>
  <mo>&lt;</mo>
  <msub><mi>log</mi><mn>2</mn></msub>
  <mrow>
    <mo>(</mo>
    <mfrac>
      <mrow><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>n</mi><mo>)</mo></mrow>
      <mrow><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo></mrow>
    </mfrac>
    <mo>)</mo>
  </mrow>
</math>

Since <math display="inline"><msub><mi>log</mi><mn>2</mn></msub><mo>(</mo><mi>cut_off</mi><mo>)</mo></math> is constant, we get <math display="inline"><mi>O</mi><mo>(</mo><mi>log</mi><mi>log</mi><mi>n</mi><mo>)</mo></math>... and that's that. Doubling (or halving) to a cutoff is <math display="inline"><mi>log</mi><mo>(</mo><mi>n</mi><mo>)</mo></math>, doing it to the exponent is <math display="inline"><mi>log</mi><mo>(</mo><mi>log</mi><mo>(</mo><mi>n</mi><mo>)</mo><mo>)</mo></math>: a quick rule of thumb to intuit this.

<hr>

Going back to sum of series, it's important to note that we're applying them to count operations and nothing else.

```ruby
total = 0
arr.each do |x|
  total += 1 if x.even?
end
total
```

Each operation takes constant time, `n` times. The count is just `n`, no series to sum. But if the inner loop depends on the outer index, the operation count is the **sum** of the varying counts.

```ruby
pairs = []
(0...arr.size).each do |i|
  (i+1...arr.size).each do |j|
    pairs << [arr[i], arr[j]]
  end
end
pairs
```

Here, the inner step does constant work ([push at end of dynamic array](https://en.wikipedia.org/wiki/Array_%28data_structure%29#Comparison_with_other_data_structures)). The `j` loop around it contributes linear time since it does a constant operation `n` times. The total work is,

<math display="inline">
  <mo>∑</mo>
  <mtext>inner runs</mtext>
  <mo>×</mo>
  <mi>O</mi><mo>(</mo><mn>1</mn><mo>)</mo>
</math>

<math display="inline">
  <mo>[</mo>
  <mo>(</mo><mi>n</mi><mo>−</mo><mn>1</mn><mo>)</mo>
  <mo>+</mo>
  <mo>(</mo><mi>n</mi><mo>−</mo><mn>2</mn><mo>)</mo>
  <mo>+</mo><mo>⋯</mo><mo>+</mo>
  <mn>1</mn><mo>+</mo><mn>0</mn>
  <mo>]</mo>
  <mo>×</mo>
  <mi>O</mi><mo>(</mo><mn>1</mn><mo>)</mo>
</math>

This is just the sum of the series <math display="inline"><munderover><mo>∑</mo><mn>1</mn><mrow><mi>n</mi><mo>−</mo><mn>1</mn></mrow></munderover></math>, which is <math display="inline"><mi>O</mi><mo>(</mo><msup><mi>n</mi><mn>2</mn></msup><mo>)</mo></math>, since,

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mn>1</mn><mo>+</mo><mn>2</mn><mo>+</mo><mn>3</mn>
  <mo>+</mo><mo>⋯</mo><mo>+</mo>
  <mo>(</mo><mi>n</mi><mo>-</mo><mn>2</mn><mo>)</mo>
  <mo>+</mo>
  <mo>(</mo><mi>n</mi><mo>-</mo><mn>1</mn><mo>)</mo>
  <mo>=</mo>
  <mfrac>
    <mrow><mi>n</mi><mo>(</mo><mi>n</mi><mo>−</mo><mn>1</mn><mo>)</mo></mrow>
    <mn>2</mn>
  </mfrac>
</math>

The same sort of thing is happening in the example from before, 

```ruby
(1...n).each do |i|
  j = i
  while j < n
    j *= 2
  end
end
```

We're summing the series <math display="inline"><munderover><mo>∑</mo><mrow><mi>i</mi><mo>=</mo><mn>1</mn></mrow><mi>n</mi></munderover><mi>log</mi><mrow><mo>(</mo><mfrac><mi>n</mi><mi>i</mi></mfrac><mo>)</mo></mrow></math> to count possible operations.

<div class="section-break">side note</div>

At some point in all of this, for the fun of it, [Nid](https://bsky.app/profile/nid90.bsky.social) and I worked out a simple algebraic expansion that feels more symmetric than blindly applying the [Gauss pairing trick](https://en.wikipedia.org/wiki/Gauss_sum), which every LLM appears to want you to do.

![apple-pencil.jpeg](/blog/images/whiteboard-sum-series.jpeg)

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mn>1</mn><mo>+</mo><mn>2</mn><mo>+</mo><mn>3</mn>
  <mo>+</mo><mo>⋯</mo><mo>+</mo>
  <mi>n</mi>
</math>

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mn>1</mn><mo>+</mo><mn>2</mn><mo>+</mo><mn>3</mn>
  <mo>+</mo><mo>⋯</mo><mo>+</mo>
  <mo>(</mo><mi>n</mi><mo>-</mo><mn>2</mn><mo>)</mo>
  <mo>+</mo>
  <mo>(</mo><mi>n</mi><mo>-</mo><mn>1</mn><mo>)</mo>
  <mo>+</mo>
  <mi>n</mi>
</math>

The key idea here is to try and turn the series into a reducible `n`. So you re-write each term in the shape of `(n+1)`.

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mo>[</mo><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo><mo>-</mo><mi>n</mi><mo>]</mo>
  <mo>+</mo>
  <mo>[</mo><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo><mo>-</mo><mo>(</mo><mi>n</mi><mo>-</mo><mn>1</mn><mo>)</mo><mo>]</mo>
  <mo>+</mo><mo>⋯</mo><mo>+</mo>
  <mo>[</mo><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo><mo>-</mo><mn>2</mn><mo>]</mo>
  <mo>+</mo>
  <mo>[</mo><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo><mo>-</mo><mn>1</mn><mo>]</mo>
</math>

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <munder>
    <mrow>
      <mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo>
      <mo>+</mo>
      <mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo>
      <mo>+</mo><mo>⋯</mo>
    </mrow>
    <mtext>n times</mtext>
  </munder>
  <mo>−</mo>
  <mo>[</mo>
  <mo>(</mo><mi>n</mi><mo>−</mo><mn>1</mn><mo>)</mo>
  <mo>+</mo>
  <mo>(</mo><mi>n</mi><mo>−</mo><mn>2</mn><mo>)</mo>
  <mo>+</mo><mo>⋯</mo><mo>+</mo>
  <mn>2</mn><mo>+</mo><mn>1</mn>
  <mo>]</mo>
</math>

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mi>n</mi><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo>
  <mo>-</mo>
  <mo>[</mo><mn>1</mn><mo>+</mo><mn>2</mn><mo>+</mo><mo>⋯</mo><mo>+</mo><mi>n</mi><mo>]</mo>
</math>

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mi>n</mi><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo>
  <mo>-</mo>
  <mi>S</mi>
</math>

<math display="inline">
  <mn>2</mn><mi>S</mi>
  <mo>=</mo>
  <mi>n</mi><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo>
</math>

<math display="inline">
  <mi>S</mi>
  <mo>=</mo>
  <mfrac>
    <mrow><mi>n</mi><mo>(</mo><mi>n</mi><mo>+</mo><mn>1</mn><mo>)</mo></mrow>
    <mn>2</mn>
  </mfrac>
</math>

This is also essentially the Gauss sum, but without an explicit pairwise reverse sum. This is more about reducing the series in a shape of `n` and finding a recursive copy of the original sum.

<div class="section-break">end of side note</div>

None of this stuff is new of course, and my previously tacit understanding of much of this isn't inherently bad either. I started off analysing snippets of code with the help of LLMs, I gave up and moved on to using it purely in a pedagogy mode and finding connections that I never knew existed.

This is my preferred way of using LLMs these days. I've been infrequently working on a [code-review plugin for emacs & magit](https://github.com/adjaecent/magit-hutch) with agents, in a pure pedagogical style. I hand-write all the code and use agents to validate, correct and guide me in the right direction.
