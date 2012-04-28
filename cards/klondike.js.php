<div class="highlight">
<pre><span class="doc">/**
 * A game of Klondike Solitaire.
 */</span>
<span class="keyword">function</span> <span class="function-name">Solitaire</span>() {
    <span class="keyword">var</span> <span class="variable-name">board</span> = <span class="keyword">new</span> Board({
        <span class="variable-name">rootId</span> : <span class="string">&quot;Klondike&quot;</span>,
        <span class="variable-name">magicalX</span> : 106
    });

    <span class="keyword">var</span> <span class="variable-name">totalDeck</span> = <span class="keyword">new</span> Deck(board.collapsedType, 0, 0, {
        <span class="comment">//never let someone drag a card to the dec</span><span class="comment">k</span><span class="comment">
</span>        <span class="function-name">filter</span> : <span class="keyword">function</span> () { <span class="keyword">return</span> <span class="constant">false</span>; },
        <span class="variable-name">draggable</span> : <span class="constant">false</span>
    });
    totalDeck.initialize(<span class="constant">true</span>);

    <span class="keyword">var</span> <span class="variable-name">discard</span> = <span class="keyword">new</span> Deck(board.collapsedType, 1, 0, {
                <span class="comment">//never let someone drag a card to the discard pil</span><span class="comment">e</span><span class="comment">
</span>		<span class="function-name">filter</span> : <span class="keyword">function</span>(<span class="js2-function-param-face">card</span>) { <span class="keyword">return</span> <span class="constant">false</span>; }
	});
    board.addDeck(discard);

    totalDeck.shuffle();
    totalDeck.setAction(<span class="keyword">function</span> () {
        <span class="keyword">var</span> <span class="variable-name">top</span> = totalDeck.peek();

        <span class="keyword">if</span> (!top) {
            discard.deal(totalDeck, discard.getCards().length, <span class="constant">true</span>);
        } <span class="keyword">else</span> {
            top.setFaceUp(<span class="constant">true</span>);
            totalDeck.remove(top);
            discard.addTop(top);
        }
    });
    board.addDeck(totalDeck);

    <span class="keyword">var</span> <span class="variable-name">pile</span>;
    <span class="keyword">var</span> <span class="variable-name">endPiles</span> = [];

    <span class="keyword">for</span> (<span class="keyword">var</span> <span class="variable-name">i</span> = 1; i &lt;= 7; i++) {
        pile = <span class="keyword">new</span> Deck(board.defaultType, i - 1, 1);
        totalDeck.deal(pile, i, <span class="constant">true</span>);

        <span class="keyword">var</span> <span class="variable-name">topCard</span> = pile.peek();
        <span class="keyword">if</span> (topCard) {
            topCard.setFaceUp(<span class="constant">true</span>);
        }

        pile.setFilter((<span class="keyword">function</span> (<span class="js2-function-param-face">pile</span>) {
             <span class="keyword">return</span> <span class="keyword">function</span> (<span class="js2-function-param-face">card</span>) {
                 <span class="keyword">var</span> <span class="variable-name">top</span> = pile.peek();

                <span class="keyword">if</span> (top) {
                    <span class="keyword">return</span> (top.getColor() != card.getColor()) &amp;&amp;
                        top.getRank() - card.getRank() ==1;
                } <span class="keyword">else</span> {
                    <span class="keyword">return</span> card.getRank() == 13;
                }
            };
        })(pile));

        <span class="comment">// Flip the top card if needed:
</span>        pile.observe((<span class="keyword">function</span> (<span class="js2-function-param-face">pile</span>) {
            <span class="keyword">return</span> <span class="keyword">function</span> (<span class="js2-function-param-face">event</span>) {
                <span class="keyword">if</span> (event.type == <span class="string">&quot;remove&quot;</span>) {
                    <span class="keyword">if</span> (pile.peek()) {
                        pile.peek().setFaceUp(<span class="constant">true</span>);
                    }
                }
            };
        })(pile));

        board.addDeck(pile);
    }

    <span class="keyword">for</span> (i = 0; i &lt; 4; i++) {
        endPiles[i] = <span class="keyword">new</span> Deck(board.collapsedType, 3 + i, 0);
        board.addDeck(endPiles[i]);
        endPiles[i].setFilter((<span class="keyword">function</span> (<span class="js2-function-param-face">pile</span>) {
            <span class="keyword">return</span> <span class="keyword">function</span> (<span class="js2-function-param-face">card</span>, <span class="js2-function-param-face">deck</span>, <span class="js2-function-param-face">size</span>) {
                <span class="keyword">if</span> (size &gt; 1) {
                    <span class="keyword">return</span> <span class="constant">false</span>;
                }

                <span class="keyword">var</span> <span class="variable-name">top</span> = pile.peek();

                <span class="keyword">if</span> (top) {
                    <span class="keyword">return</span> (top.getSuit() == card.getSuit()) &amp;&amp;
                        (card.getRank() - top.getRank() == 1);
                } <span class="keyword">else</span> {
                    <span class="keyword">return</span> card.getRank() == 1;
                }
            };
        })(endPiles[i]));
    }
}
$(document).ready(<span class="keyword">function</span>() { <span class="keyword">new</span> Solitaire(); });</pre>
</div>
