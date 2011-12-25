  /* This is the script that creates the board and pieces; lets people move the
   pieces about while making sure that all the moves are legal. This part was
   written by Tikhon Jelvis.*/

   AI_DIFFICULTY = 3;
   EFFECTS_ON = true;
   DEBUG_MODE = false;

   if (DEBUG_MODE) {
       try {
	   console.log("Debugging mode is on!");
       } catch (error) {
	   console = {
	       log : function (input) {
		   alert(input);
	       }
	   };
       }
   }

//The Game class runs everything. 
   var Game = Class.create({
       isSinglePlayerGame : false,

       container : new Element("div", {"class": "container"}),
       axisLabels : [],

       castlingInfo : new Hash({
	   "wK" : false,
	   "bK" : false,
	   "wRr" : false,
	   "bRr" : false,
	   "wRl" : false,
	   "bRl" : false
       }),
       enPassant : null,

       turn : "white",

       selectedPiece : null,
       lastMove : null,

       /* Creates the game, setting everything up and starting a new game.
          The argument specifies the div in which the game needs to be. 
          An element to hold the game can be specified; by default, the
          game is inserted into the body. */
       initialize : function (holder) {
	   //Event management: listeners, events, firing... fun.
	   var listeners = [];//List of listeners interested in the game, to be called when the state changes.

	   //These next two variables toggle whether each side can be controlled by a local player:
	   var sidesOn = {
	       "black" : true,
	       "white" : true
	   };

	   if (holder) {
	       this.holder = $(holder);
	   } else {
	       this.holder = $$("body")[0];
	   }

	   this.holder.insert(this.container);

	   this.setUpBoard();//Now we set up the board, creating all of the squares.

	   /* Adds the given listener to the list, to be notified if there is a change in game state. This does not do
              any checks of the listener's validity. */
	   this.addGameStateListener = function (listener) {
	       listeners.push(listener);
	   };

	   /*  Removes the specified listener from the list of listeners if it is there, doing nothing otherwise. */
	   this.removeGameStateListener = function (listener) {
	       for (var i = 0; i < listeners.length; i++) {
		   if (listeners[i] === listener) {
		       listeners[i] = null;
		       listeners = listeners.compact();
		   }
	       }
	   };

	   /* Fires the specified event, notifying all of the listeners about it. The argument tells the event
              whether it was caused by a game reset (true) or a normal move (false/null/undefined...).*/
	   this.fireGameStateChanged = function (newGame) {
	       //Create the event for the current instance. The only thing the caller can change is the newGame flag.
	       var event = new GameStateChangedEvent(this.getOldState(), this.getGameState(), this.lastMove, 
						 this.turn, newGame);
	       
	       for (var i = 0, len = listeners.length; i < len; ++i) {
		   try {//We don't know if it's actually a function:
		       listeners[i](event);
		   } catch (error) {

		       console.log(error);
		       listeners.splice(i, 1);//Removes the faulty listener.
		   }
	       }
	   };

	   /* Returns whether the specified side is on. If a side is on, it is controlled by a player locally.
              otherwise, the side is controlled by something else, hopefully, or nothing at all: the player,
              either way, cannot control it. If the given side is not valid, undefined will be returned. */
	   this.isSideOn = function (side) {
	       return sidesOn[side];
	   };

	   /* Toggles whether a particular side is on. The side supplied has to be either "black" or
              "white"; otherwise, nothing happens. */
	   this.toggleSide = function (side) {
	       if (side == "black" || side == "white") {
		   sidesOn[side] = !sidesOn[side];
	       }

	       //Now we make sure to deselect the piece if it's the turned off side's turn:
	       if (side == this.turn) {
		   this.setSelectedPiece(null);
	       }
	   };

	   /* Sets the given side on or off. If the side is already in the specified state, nothing happens.
              If any of the given arguments are not valid, nothing happens. The state must be a boolean--true
              means that the side should be turned on; false--off. The side has to be represented by a string
              that begins with either "w" or "b", corresponding to white and black, respectively. Case does
              not matter. */
	   this.setSideOn = function (side, state) {
	       side = side.toLowerCase();
	       side = side.substring(0, 1);

	       if ((side != "w" && side != "b") || typeof state != "boolean") {
		   return;
	       }

	       if (this.isSideOn(side) != state) {
		   this.toggleSide(side);
	       }
	   };

           this.setUpPieces();//Creates and places all of the pieces.
	   
 	   this.setUpMenu();//The menu on the bottom:


	   //Sidebars:
           this.setUpJail();//The right one
	   this.setUpHistory();//The left one
       },

       /* Creates the board, which is a bunch of Squares of alternating colors
          all properly arranged, as they would be on an actual chess board. 
          Also creates the axis labels that mark the files and ranks by name.*/
       setUpBoard : function () {
	   var board = new Element("div", {"class": "board"});//The board itself!
	   var squares = [];//A hash of squares. Each square can be accessed using the getSquare method.

	   var letters = $A($R("a", "h"));//An array of letters that name files.

	   var color;//The color of the squares being created.
	   var squareId;//The id of the square currently being created.
	   var square;//A square to create.

	   var label;//The label of a rank or a file.
	   var fileLabelHolder = new Element("dir", {"class" : "fileLabelHolder"});

	   //This loop creates and places all the squares:
	   for (var row = 0; row < 8; ++row) {//Loop through row by row
	       for (var col = 0; col < 8; ++col) {//...and column by column within each row.
		   squareId = letters[col];
		   squareId += 8 - row;//Accounts for the fact that we're going backwards.
		   //We're going backwards because of the fact that white is on the bottom and because
		   //of how divs stack.

		   if (row % 2 == 1) {//If it's an odd row and the white square is first:
		       if (col % 2 == 0) {
			   color = "black";
		       } else {
			   color = "white";
		       }
		   } else {//If it's an even row and the black square is first:
		       if (col % 2 == 0) {
			   color = "white";
		       } else {
			   color = "black";
		       }
		   }

		   square = new Square(color, squareId, this);
		   board.insert(square.element);
		   squares.push(square);

		   if (row == 7) {//The first row, on the bottom, gets labels.
		       label = new Element("div", {"class" : "file axisLabel"});
		       label.update(letters[col]);
		       fileLabelHolder.insert(label);
		       this.axisLabels.push(label);
		   }
	       }
	       
	       // Now we create the row's label on the axis:
	       label = new Element("dir", {"class" : "rank axisLabel"});
	       label.update(8 - row);// Remember, we're going backwards!
	       board.insert(label);
	       this.axisLabels.push(label);
	   }

	   //Now we insert the file labels, which are all inside the fileLabelHolder div:
	   board.insert(fileLabelHolder);

	   //And now we make the two intial variables properties of this game:
	   this.board = board;
	   this.squares = squares;

	   // The file label holder gets inserted now.
	   this.container.insert(this.board);//Puts the board into the container.

	   /* Returns the square at the specified position or null if there is none. */
	   this.getSquare = function (position) {
	       var index;//The numerical index of the square inside the squares array.

	       if (!ChessUtil.isPositionValid(position)) {
		   return null;
	       }

	       index = letters.indexOf(position.substring(0, 1));
	       index += 8 * (8 - position.substring(1));
	       return squares[index];
	   };
       },

       /* This function creates and place all of the pieces. */
       setUpPieces : function () {
	   var letters = $A($R("a", "h"));//An array of letters corresponding to files!
	   //The order of pieces along the back row:
	   var backRowPieceOrder = ["R", "N", "B", "Q", "K", "B", "N", "R"];

	   var file;//The current file.
	   var type;//The current non-pawn type to create.

	   for (var i = 0; i < 8; i++) {//Loop through to create the pieces:
	       file = letters[i];//We go file by file; this is the file as a letter.
	       type = backRowPieceOrder[i];//The type of (non-pawn) piece for this row.
	       
	       //White pieces:
	       new Piece("p", "white", file + 2, this);//Pawn
	       new Piece(type, "white", file + 1, this);//Piece

	       //Black pieces:
	       new Piece("p", "black", file + 7, this);//Pawn
	       new Piece(type, "black", file + 8, this);//Piece
	   }
       }, 

       /* Creates the jail, where pieces go after they're captured. */
       setUpJail : function () {
	   //The actual sidebar:
	   var jail = new Sidebar({className : "jail", direction : "left",  position: 107, distance : 195});
	   var pieces = [];//An array of pieces for convenient clearing. And stuff. Yeah.

	   jail.tab.setAttribute("title", "Toggle the jail");

	   //The tab needs to be updatable:
	   jail.tab.update = function () {
	       var black = this.getPointsOnBoard("black");
	       var white = this.getPointsOnBoard("white");
	       var blackFontSize = 12;// The default font size for each is 12, obviously...
	       var whiteFontSize = 12;

	       jail.tab.blackCell.update(black);
	       jail.tab.whiteCell.update(white);

	       if (black >= 100) {//The text needs to be smaller in order to fit properly.
		   blackFontSize = 10;
		   console.log("[ui] Black cell font size should shrink!");
	       }
	       if (white >= 100) {
		   whiteFontSize = 10;
		   console.log("[ui] Black cell font size should shrink!");
	       }

	       jail.tab.blackCell.setStyle("font-size : " + blackFontSize + "pt");
	       jail.tab.whiteCell.setStyle("font-size : " + whiteFontSize + "pt");
	   }.bind(this);

	   jail.tab.update();//The tab gets updated immediately.
	   this.addGameStateListener(jail.tab.update);//Now we make it update whenever the game state changes.

	   /*Adds a piece to the jail. If the piece is null, nothing happens. */
	   jail.addPiece = function (piece) {
	       if (!piece) {//If the piece is null or the like:
		   return;
	       }
	       
	       var color = piece.color;
	       pieces.push(piece);
	       
	       //Put the piece in the appropriate array and the appropriate portion:
	       if (color == "white") {
		   this.whitePortion.insert(piece.element);
	       } else {
		   this.blackPortion.insert(piece.element);
	       }
	   };

	   /* Removes all the pieces in the jail from the DOM. Basically, they die. Forever. Mourn them! */
	   jail.clear = function () {
	       var piece;//Each piece in the jail.

	       for (var i = 0, len = pieces.length; i < len; ++i) {
		   piece = pieces[i];
		   piece.remove();
	       }

	       pieces.clear();
	   };

	   //And now we put the jail into the board:
	   this.holder.insert(jail.element);
	   
	   this.jail = jail;
       },

       /* Sets up the history side bar to record the game's moves. */
       setUpHistory : function () {
	   //The actual sidebar:
	   var history = new Sidebar({className : "history", direction : "right",  position : 107, distance : 195}); 
	   history.tab.setAttribute("title", "Toggle the history");

	   var whiteList = new Element("ol");//The list of white moves which gets a number for each turn.
	   var blackList = new Element("ul");//The non-numbered list of black moves.

	   var turnNumber = 1;//No nice zero-based indexing in the real world...

	   var upButton = new Element("div", {"class" : "disabled average button"});//The button to scroll up.
	   upButton.insert(new Element("img", {"src" : "img/upTriangle.png", "alt" : "Up"}));

	   var downButton = new Element("div", {"class" : "disabled average button"});//The button to scroll down.
	   downButton.insert(new Element("img", {"src" : "img/downTriangle.png", "alt" : "Down"}));

	   function setEnabled(enabled) {//Toggles whether the button is enabled.
	       if (enabled) {
		   this.removeClassName("disabled");
		   this.addClassName("enabled");
		   this.enabled = true;
	       } else {
		   this.removeClassName("enabled");
		   this.addClassName("disabled");
		   this.enabled = false;
	       }
	   };

	   upButton.setEnabled = setEnabled.bind(upButton);
	   downButton.setEnabled = setEnabled.bind(downButton);

	   var scrolling = false;
	   // Offset has to be a number.
	   function scroll(offset) {
	       console.log("[ui] Scrolling");

 	       var elements = [history.blackPortion.childElements(), history.whitePortion.childElements()];
	       
	       scrolling = true;

	       elements.each(function (elements) {
		   elements.each(function (element) {
		       console.log(element);

		       var margin = element.getStyle("margin-top");

		       if (margin === null) { //null means 0, really.
			   margin = 0;
		       } else {
			   margin = margin.replace(/[^-\.\d]/g,"") * 1; //Made into a number.
		       }

		       margin += offset;
		       console.log("[ui] New margin: " + margin + "px");

		       
		       element.morph("margin-top : " + margin + "px",
				     {afterFinish : (function () {
					 scrolling = false;
					 updateScrollButtons();
				     }).bind(this)}); //The actual moving. 
		   });
	       });
	   };

	   /* Checks which of the scroll buttons should be enabled and enables them as necessary.
              "Necessary" is a word that always annoys me.*/
	   function updateScrollButtons () {
	       var height = whiteList.getHeight();
	       var offset = whiteList.getStyle("margin-top");

	       if (offset) {
		   offset = offset.replace(/[^-\.\d]/g, "") * 1;
		   height += offset;
		   
		   // Enabled the up button if the top is above 0 (the margin is negative):
		   upButton.setEnabled(offset < 0);
	       }

	       // Enable the down button if the bottom of the list is at the edge:
	       downButton.setEnabled(
		   height >= history.element.getHeight() - 20);// A magic number in the wild.
	   }

	   upButton.observe("click", (function () {
	       if (this.enabled && !scrolling) {
		   console.log("[ui] Up scrolling button enabled.");

		   // Let's scroll up half the history's height.
		   var offset = history.element.getHeight() / 2;
		   (scroll.bind(this, offset))();
	       }
	   }).bind(upButton));	

	   downButton.observe("click", (function () {
	       if (this.enabled && !scrolling) {
		   console.log("[ui] Down scrolling button enabled.");

		   // Let's scroll down half the history's height.
		   var offset = -history.element.getHeight() / 2;
		   (scroll.bind(this, offset))();
	       }
	   }).bind(downButton));   

	   history.tab.update = function () {
	       var fontSize = 12;//The default font size is 12.

	       if (turnNumber >= 100) {//Three digits don't fit properly.
		   fontSize = 10;
		   console.log("[ui] History tab font size should be smaller, eh.");
	       } 
	       
	       history.tab.whiteCell.setStyle("font-size : " + fontSize + "pt");

	       history.tab.whiteCell.update(turnNumber);
	       history.tab.blackCell.update(this.turn.substring(0, 1).toUpperCase());
	   }.bind(this);
	   history.tab.update();//The tab gets updated immediately.

	   history.whitePortion.insert(whiteList);
	   history.blackPortion.insert(blackList);

	   upButton.setStyle({top : 0});
	   downButton.setStyle({bottom : 0});

	   history.element.insert(upButton);
	   history.element.insert(downButton);

	   history.update = function (event) {
	       var li;//The actual list element for this update.
 	       var move;//The move that needs to be accounted for.
	       var state;//The state *before* the current move.
	       var height;//The effective height of the list--the height plus the top offset.
	       var offset;//The offset from the top.

	       //Check for odd conditions:
	       if (event.isNewGame()) {//If the game was restarted, clear this tab.
		   //Clear the history:
		   blackList.update();
		   whiteList.update();

		   turnNumber = 1;//Resets the turn number when the game is restarted.
		   history.tab.update();

		   //We need to scroll the list up to the top...
 		   var elements = [history.blackPortion.childElements(), history.whitePortion.childElements()];
		   elements.each(function (elements) {
		       elements.each(function (element) {
			   element.setStyle("margin-top : 10px");
		       });
		   });
				    

		   updateScrollButtons();
		   return;
	       }

	       move = event.getMove();
	       state = event.getOldState();

	       if (!move) {//If no move is provided, don't do anything:
		   return;
	       }

	       li = new Element("li");
	       move = ChessUtil.toAlgebraicNotation(state, move);//The new move in algebraic notation.
	       
	       li.update(move);

	       //Put it into the appropriate list:
	       if (this.turn == "white") {
		   //If it tries to put an element in the black list which would make it longer than the 
                   //white list, it should quit:
		   if (blackList.childElements().length >= whiteList.childElements().length) {
		       return;
		   }

		   blackList.insert(li);
		   turnNumber++;//If it's white's turn now, it's a new turn.
	       } else {
		   whiteList.insert(li);
	       }
	       
	       //The scroll buttons get updated:
	       updateScrollButtons();

	       //Now we update the tab so that it has the righ turn number and color.
	       history.tab.update();
	   }.bind(this);

	   this.addGameStateListener(history.update);

	   this.holder.insert(history.element);
	   
	   this.history = history;
       },

       /* Sets up the little menu on the bottom-right with the new game buttons. */
       setUpMenu : function() {
	   var menu = new Element("div", {"class" : "menu"});
	   var aiGameButton = new Element("div", {"class" : "black button",
	       "title" : "Start a new game against the computer"});
	   aiGameButton.update("AI Game");
	   var moveMade;//The listener for the ai.

	   function newAiGame() {
	       this.reset();
	       this.setSideOn("black", false);
	       this.isSinglePlayerGame = true;
	       // TODO: Difficulty needs to be adjustable!!!
	       var ai = ChessAI.makeAIPlayer(AI_DIFFICULTY);
	       var turn = "black";
	       
	       //If there is a listener already, remove it:
	       if (moveMade) {
		   this.removeGameStateListener(moveMade);
	       }

	       /* This function responds to moves made, giving them to the ai and getting
                  a response. */
	       moveMade = function (event) {
		   //To account for a game's being restarted:
		   if (event.isNewGame()) {
		       ai = ChessAI.makeAIPlayer(AI_DIFFICULTY);
		   }

		   if (event.getTurn() != turn) {
		       return;//If it's the wrong turn, do nothing.
		   }

		   if (DEBUG_MODE) {
		       console.log("[ai-wrapper] Making a move in response to: ");
		   }
		   var move = event.getMove();
		   if (DEBUG_MODE) {
		       console.log("[ai-wrapper] " + move);
		   }
		   
		   try {
		       var response = ai(move);
		   } catch (error) {
		       if (DEBUG_MODE) {
			   console.log("[ai-wrapper] Caught an ai error.");
		       }
		       if(error.getMessage() === "[AI error] Error: Game is already over.") {
			   if (DEBUG_MODE) {
			       console.log("[ai-wrapper] AI thinks the game is over.");
			   }
			   if (!this.isGameOver) {
			       throw error;
			   } //Do nothing otherwise.
		       } else {
			   throw error;
		       }
		   }
		   if (DEBUG_MODE) {
		       console.log("[ai-wrapper] " + response);
		   }
		   this.makeMove(response);
		   if (DEBUG_MODE) {
		       console.log("[ai-wrapper] Turn made");
		   }
	       }.bind(this);
	       this.addGameStateListener(moveMade);
	   };
	   aiGameButton.observe("click", newAiGame.bind(this));

	   var newGameButton = new Element("div", {"class" : "white button",
	       "title" : "Start a new game"});
	   newGameButton.update("New Game");

	   var newGame = (function () {
	       //If there is an ai listener, remove it:
	       if (moveMade) {
		   this.removeGameStateListener(moveMade);
	       }

	       this.isSinglePlayerGame = false;

	       this.reset();
	       this.setSideOn("white", true);
	       this.setSideOn("black", true);
	   }).bind(this);
	   newGameButton.observe("click", newGame.bind(this));

	   menu.insert(aiGameButton);
	   menu.insert(newGameButton);

	   this.holder.insert(menu);
       },

       /* Returns the amount of points the specified color has on the board currently. */
       getPointsOnBoard : function (color) {
	   //The point value of each of the piece types:
	   var pointValue = new Hash({
	       "p" : 1,
	       "N" : 3,
	       "B" : 3,
	       "R" : 5,
	       "Q" : 9
	   });

	   var points = 0;//The total number of points for the specified side.
	   
	   var piece;//The piece currently being counted.
	   var type;//The type of said piece.

	   if (color=="w" || color=="b") {//To let people supply just one letter, if that's what they want:
	       color = (color == "w") ? "white" : "black";
	   }

	   for (var i = 0, squares = this.squares, len = squares.length; i < len; ++i) {
	       piece = squares[i].piece;
	       if (piece) {//If there is a piece and all that...
		   if (piece.color == color) {
		       type = piece.getType();
		       if (pointValue.get(type)) {
			   points += pointValue.get(type);
		       }
		   }
	       }
	   }

	   return points;
       },
	 
       /* Promotes the specified piece by showing the player a menu where they can choose what piece
          the pawn will now become. If the specified piece is, for some reason, not valid, nothing
          will happen. */
       promote: function (piece) {
	   var color;//The color of the piece to promote.

	   //The piece's physical position:
	   var top;
	   var left;

	   //Actual elements:
	   var menu;//The menu itself.
	   var background;//The menu's transparent background.
	   var clickShield;//The div that blocks clicking on the rest of the game.

	   //The function which actually promotes the peice:
	   function promoteTo(type) {
	       piece.setType(type);//Actually change the piece's type
	       menu.shrink({//Now we actually get rid of the menu.
		   afterFinish : function () {
		       menu.remove();
		   }
	       });
	       clickShield.remove();//Getrs rid of the click shield.	       this.lastMove += "*" + type;
	       this.lastMove += "*" + type;
	       this.changeTurn();//Change the turn.
	   }; 

	   var promotions = ["Q", "R", "B", "N"];//The pieces that can be promoted to.
	   var type;//The type of piece to promote to, on the menu.
	   var src;//The src for the img tag in the menu.
	   var img;//The actual img in the menu.

	   if (!piece || piece.getType() != "p") {//If the specified piece doesn't exist or isn't a pawn:
	       return;//Nothing happens!
	   }

	   //Deselect the piece to be promoted, by setting the game's selected peice to null:
	   this.setSelectedPiece(null);

	   //The piece's color:
	   color = piece.color.substring(0, 1);

	   //Now we get the piece's physical position:
	   top = piece.element.viewportOffset().top;
	   left = piece.element.viewportOffset().left;

	   //The actual menu element:
	   menu = new Element("div", {"class": "promMenu"});
	   menu.setStyle({"position": "absolute", "top": top + "px", "left": left + "px"});
	   menu.hide();//Make it hidden when it gets inserted.

	   //Now the menu background:
	   background = new Element("div", {"class": "promMenuTrans"});
	   background.setOpacity(0.5);//Make it semi-transparent.
	   background.setStyle({"position": "absolute"});
	   menu.insert(background);

	   $$("body")[0].insert(menu);//Yay for css selectors!

	   //The following element makes everything on the board except the menu unclickable:
	   clickShield = new Element("div", {"class": "menuBackground"});
	   clickShield.setOpacity(0.5);
	   this.container.insert(clickShield);

	   //And now we create the actual promotion options:
	   for (var i = 0; i < 4; i++) {
	       type = promotions[i];
	       src = "img/" + color + type + ".png";

	       img = new Element("img", {"src": src, "class": "prom"});
	       menu.insert(img);
	       
	       img.observe("click", promoteTo.bind(this, type));
	   }

	   //Finally, we can make the promotion menu appear:
	   menu.grow();
       },

       /* Changes the turn from white to black and vice versa. This both changes the turn from the
          game's perspective--the opposite side gets to move its pieces--and does some things with
          the actual interface, namely changing the background, getting rid of any selected squares
          and checking to see if the game is over due to a checkmate or a stalemate. If the argument
          is not falsey, then the event after the change of background is fired as a "new game" type
          of event.*/
       changeTurn : function (newGame) {
	   var newColor;//The new background color for the next turn.
	   var oldColor;//The color that the background used to be.

	   var hasMoves;//Whether the turn whose side it is has any legal moves.
	   var state;//The current game state.
	   var turn;//The curretn turn.

	   this.turn = (this.turn == "white") ? "black" : "white";

	   //If the turn became white again, the turn number needs to be incremented:
	   if (this.turn == "white") {
	       this.turnNumber++;
	   }

	   //Make sure no piece is selected:
	   this.setSelectedPiece(null);
	   //This also clears all the previously selected squares.

	   //And now we check for end conditions. The game ends when the current side has no moves.
	   hasMoves = false;
	   state = this.getGameState();
	   turn = this.turn;//Scoping reasons...

	   state.each(function (pair) {
	       var position;//The current piece's position.
	       var color;//The first letter of the current piece's color.

	       var oppositeColor;//The opposite of the new color.

	       if (!pair.value) {//No piece:
		   return;
	       }

	       position = pair.key;
	       color = pair.value.substring(0, 1);

	       if (color == turn.substring(0, 1)) {//It's a piece of the right color:
		   hasMoves = hasMoves || ChessUtil.possibleMoves(state, position, {hasMoves : true});
	       }
	   });

	   if (!hasMoves) {//The game is over:
	       if (ChessUtil.checkForCheck(state, this.turn)) {
		   oppositeColor = (this.turn == "white") ? "black" : "white";
		   alert("Checkmate! " + oppositeColor + " wins!");
	       } else {
		   alert("Stalemate!");
	       }
	       this.isGameOver = true;
	   }

	   //As the game state must have changed whenever the turn does:

	   //Now we change the background to match the current turn's color:
	   if (EFFECTS_ON) {
	       if (this.isSinglePlayerGame) {
		   var gameChanged = (function () {
		       var turn;
		       if (newColor == "#FFF") {
			   turn = "white";
		       } else {
			   turn = "black";
		       }

		       if (this.isSinglePlayerGame && turn == this.turn) {
			   if (newGame) {
			       this.fireGameStateChanged(true);
			   } else {
			       this.fireGameStateChanged();
			   }
		       }
		   }).bind(this);

		   newColor = (this.turn=="white") ? "#FFF" : "#000";
		   this.container.morph("background: " + newColor, {afterFinish : gameChanged});
		   oldColor = (newColor == "#FFF") ? "#000" : "#FFF";
		   this.container.morph("background: " + newColor);
		   this.axisLabels.each(function (label) {
		       label.morph("color : " + oldColor);
		   });
		   
	       } else {
		   newColor = (this.turn=="white") ? "#FFF" : "#000";
		   oldColor = (newColor == "#FFF") ? "#000" : "#FFF";
		   this.container.morph("background: " + newColor);
		   this.axisLabels.each(function (label) {
		       label.morph("color : " + oldColor);
		   });

		   if (!newGame) {
		       this.fireGameStateChanged();
		   }
	       }
	   } else {
	       if(!newGame) {
		   this.fireGameStateChanged();
	       }
	   }
       },

       /* Sets the turn to white, calling changeTurn() if it is currently black's turn. */
       resetTurn : function () {
	   if (this.turn == "black") {
	       this.changeTurn(true);
	   }
       },

       /* Clears all the selected squares. */
       clearSelectedSquares : function () {
	   var square;//The square to clear.

	   for (var i = 0, squares = this.squares, len = squares.length; i < len; ++i) {
	       square = squares[i];
	       square.dehighlight(true);//Dehighlights COMPLETELY
	   }
       },

       //------------------------------Actual Gameplay Stuff:------------------------------//
       /* Sets the selected piece, making sure only one is selected at a time and taking care
          of the visual selecting the pieces use. Once the piece is selected, all its
          possible moves are found and shown on the board; when it is deselected, any shown
          moves are hidden. */
       setSelectedPiece : function (piece) {
	   var moves;//The set of possible moves for the selected piece.

	   this.clearSelectedSquares();

	   if (!piece) {
	       if (this.selectedPiece) {
		   this.selectedPiece.setSelected(false);//Deselect any selected piece.
	       }

	       this.selectedPiece = null;
	   } else {
	       if (this.selectedPiece) {//Makes sure the old piece is not selected any more.
		   this.selectedPiece.setSelected(false);
	       }
	       this.selectedPiece = piece;
	       piece.setSelected(true);//Makes sure the new selected piece is shown as such.

	       //Now we get and show the moves of the newly selected piece:
	       moves = ChessUtil.possibleMoves(this.getGameState(), piece.getPosition());
	       if (moves) {//If there are any moves to consider:
		   moves.each(function(posMove) {
		       var rank;//The current piece's rank.
		       var color;//The current piece's color.

		       if (!ChessUtil.isPositionValid(posMove)) {//Dealing with abnormal positions:
			   rank = piece.getPosition().substring(1);
			   color = piece.color.substring(0, 1);

			   //Dealing with castling:
			   if (posMove == "0-0" && this.canCastle("0-0")) {
			       this.getSquare("g" + rank).specialMove = "0-0";
			       this.getSquare("g" + rank).highlight();
			   }

			   if (posMove == "0-0-0" && this.canCastle("0-0-0")) {
			       this.getSquare("c" + rank).specialMove = "0-0-0";
			       this.getSquare("c" + rank).highlight();
			   }
			   return;
		       }

		       this.getSquare(posMove).highlight();
		   }.bind(this));
	       }
	   }
       },

       /* Returns whether the king whose turn it is can currently castle the specified way ("0-0-0" or "0-0"). */
       canCastle : function (castle) {
	   //The castle supplied must be one of the two valid patterns:
	   if (! /(0\-0|0\-0\-0)/.test(castle)) {
	       return false;
	   }

	   var kingPos = null;
	   var color = this.turn.substring(0, 1);
	   var kingMoved = this.castlingInfo.get(color + "K");
	   var currentState = this.getGameState();
	   var king = color + "K";

	   var moves;//The king's possible moves.

	   //Now we loop through to find the appropriate king:
	   currentState.each(function(pair) {
	       var piece = pair.value;

	       if (piece == king) {
		   kingPos = pair.key;
	       }
	   });

	   if (!kingPos) { //Obviously can't castle if there is not king, somehow...
	       return false;
	   }

	   moves = ChessUtil.possibleMoves(this.getGameState(), kingPos);
	   if (!moves || moves.indexOf(castle) < 0) {
	       return false;//The castling is not a valid move!
	   }

	   if (castle == "0-0") {
	       return !kingMoved && !this.castlingInfo.get(this.turn + "Rr");
	   } else { 
	       return !kingMoved && !this.castlingInfo.get(this.turn + "Rl");
	   }
       },

       /* Makes the specified move. This method will do nothing if the piece's color does not match the turn.
          or if the given move is not valid. Leading and trailing whitespace is ignored and the delimitter
          between the initial square and the final square can be any one character that is not used otherwise.*/
       makeMove : function (move) {
	   var changeTurn = true;//Change the turn when done with everything else?

	   var rank;//The current turn's color's back rank.
	   var king;//Said color's king.
	   var rook;//Said color's rook.

	   var moveLegal;//Is this move legal?
	   var piece;//The piece to be moved.
	   var newPosition;//The piece's new position.

	   var posMoves;//The set of possible moves for the piece.

	   var enPassantSet;//Is en passant going to be an option?
	   var distance;//The distance the pawn just moved, in squares. (Can only be 1 or 2, really).
	   var dir;//The direction the pawn just took en passant.
	   var capturedPos;//The position that got captured.

	   var promoteTo = null;//The piece type to promote to, explicitly.

	   //Trim leading and trailing whitespace, which is ignored:
	   move = move.replace(/^\s*/,"");
	   move = move.replace(/\s*$/,"");

	   this.oldState = this.getGameState();

	   //Next we check for castling, the one possible move not conforming to the normal standard:
	   if (move == "0-0") {//King-side castling:
	       this.lastMove = move;
	       if (!this.canCastle("0-0")) {//If castling is not valid:
		   if (DEBUG_MODE) {
		       console.log("[ui-rules-checking] Castling does not seem to be legal.");
		   }
		   return;
	       }
	       
	       rank = this.turn=="white" ? 1 : 8;//The turn's color's back rank.
	       king = this.getSquare("e" + rank).piece;
	       rook = this.getSquare("h" + rank).piece;

	       this.movePiece(king, "g" + rank);
	       this.movePiece(rook, "f" + rank);

	       this.changeTurn();
	   }

	   if (move == "0-0-0") {
	       this.lastMove = move;
	       if (!this.canCastle("0-0-0")) {//If castling is not valid:
		   if (DEBUG_MODE) {
		       console.log("[ui-rules-checking] Castling does not seem to be legal.");
		   }
		   return;
	       }

	       rank = this.turn=="white" ? 1 : 8;//The turn's color's back rank.
	       king = this.getSquare("e" + rank).piece;
	       rook = this.getSquare("a" + rank).piece;

	       this.movePiece(king, "c" + rank);
	       this.movePiece(rook, "d" + rank);

	       this.changeTurn();
	   }

	   //Now we check whether a piece has been promoted explicitly:
	   if (/[a-h][1-8]\-[a-h][1-8]\*[RNBQ]/i.test(move)) {
	       promoteTo = move.substring(6);
	       promoteTo = promoteTo.toUpperCase();
	       move = move.substring(0, 5);
	   }

	   this.lastMove = move;

	   move = move + "";//Let's make sure it's a string.
	   move = move.split("-");//Here we create an array with the starting and ending positions, for convenience.
	   
	   moveLegal = (ChessUtil.isPositionValid(move[0]) 
			&& ChessUtil.isPositionValid(move[1]));//Both positions have to be defined and valid.

	   piece = ChessUtil.getPiece(this.getGameState(), move[0]);
	   newPosition = move[1];//The new position to move the piece to.	   

	   if (!piece) {//If there is not piece at the starting position:
	       moveLegal = false;
	   }

	   moveLegal = moveLegal && (piece.color.substring(0, 1) != this.turn);//If it's the wrong turn...

	   //Is the move legal within the current game state? Now we check this:
	   posMoves = ChessUtil.possibleMoves(this.getGameState(), move[0]);
	   moveLegal = moveLegal && (posMoves.indexOf(newPosition) >= 0);

	   if (!moveLegal) {//We can't do anything with an illegal move.
	       if (DEBUG_MODE) {
		   console.log("[ui-rules-checking] Move not legal, it seems.");
	       }
	       return;
	   }

	   //Here, we account for the possibility of a pawn taking another en passant:
	   if (piece.type == "p" && newPosition == this.enPassant) {
	       dir = (this.turn == "white") ? "s" : "n";
	       capturedPos = ChessUtil.nextPosition(this.enPassant, dir);
	       this.getSquare(capturedPos).piece.setPosition(null);
	       this.getSquare(capturedPos).setPiece(null);
	   }

	   //First we move the required piece:
	   piece = this.getSquare(piece.position).piece;//Convert the piece to an actual game piece rather than just information...
	   this.movePiece(piece, newPosition);

	   //This next section is about en passant, only valid for pawns:
	   enPassantSet = false;//If it doesn't get set here, it needs to be null again.
	   if (piece.getType() == "p") {//It has to be a pawn:
	       distance = move[1].substring(1) - move[0].substring(1);
	       
	       if (distance == 2) {//If it skipped a square moving up (e.g., it's white)
		   this.enPassant = ChessUtil.nextPosition(move[0], "n");
		   enPassantSet = true;
	       }
	       
	       if (distance == -2) {//If it's moving down (black):
		   this.enPassant = ChessUtil.nextPosition(move[0], "s");
		   enPassantSet = true;
	       }
	   }
	   
	   if (!enPassantSet) {//No en passant availible.
	       this.enPassant = null;
	   }

	   //And now we account for yet another special move, promotion:
	   rank = newPosition.substring(1);
	   if (promoteTo) {
	       piece.setType(promoteTo);
	       this.lastMove += "*" + promoteTo;
	   } else if (piece.getType() == "p" && (rank == 1 || rank == 8)) {
	       this.promote(this.getSquare(newPosition).piece);
	       changeTurn = false;
	   }

	   //Now we change the turn:
	   if (changeTurn) {
	       this.changeTurn();
	   }
       },

       /* Moves the specified piece to the new position. If either of the arguments is invalid, does nothing.*/
       movePiece : function (piece, newPosition) {
	   var key;//The piece as a string.
	   var file;//The piece's file.
	   var rank;//The piece's rank.
	   var startingRank;//The current color's starting rank.

	   //If the position isn't valid:
	   if (! ChessUtil.isPositionValid(newPosition)) {
	       return;
	   }

	   //Updating castling info:
	   if (piece.getType() == "R" || piece.getType() == "K") {//If it's an applicable piece:
	       key = piece.toString();

	       if (piece.getType() == "R") {//If it's a rook:
		   file = piece.getPosition().substring(0, 1);
		   rank = piece.getPosition().substring(1);
		   startingRank = (piece.color == "white") ? 1 : 8;

		   if (file=="a" && rank == startingRank) {
		       key += "l";
		   }

		   if (file=="h" && rank == startingRank) {
		       key += "r";
		   }
	       }

	       if (!this.castlingInfo.get(key)) {//If it hasn't moved yet, now it has:
		   this.castlingInfo.set(key, true);
	       }
	   }

	   //Deselect the piece:
	   piece.setSelected(false);

	   //Remove the piece from its old square:
	   this.getSquare(piece.getPosition()).setPiece(null);

	   //Set the piece to it's new location:
	   this.getSquare(newPosition).setPiece(piece);
       },

       /* Removes the specified piece from the board; if the piece is not there, nothing happens. */
       removePiece : function (piece) {
	   if (getSquare(piece.getPosition()).piece == piece) {//If it actually exists where it says it does:
	       this.getSquare(piece.getPosition()).setPiece(null);
	       piece.setPosition(null);
	   }
       },

       /* Resets the game, starting a new one. What fun! */
       reset : function () {
	   //First, we actually reset the game, putting the pieces into starting configuration and all that.
	   this.resetTurn();//Can't have black starting, can we?
	   this.enPassant = null;//No en passant possible, thank you very much.
	   this.setSelectedPiece(null);//Clears any old move options.
	   this.oldMove = null;
	   this.turnNumber = 1;
	   this.isGameOver = false;

	   //Reseting all the castling info:
	   this.castlingInfo.each(function (pair) {
	       this.castlingInfo.set(pair.key, false);
	   }.bind(this));

	   for (var i = 0, squares = this.squares, len = squares.length; i < len; ++i) {
	       var piece = squares[i].piece;
	       if (piece) {
		   piece.remove();//This both tells the square it's gone and removes it.
	       }
	   }

	   this.jail.clear();

	   //To set up new pieces:
	   this.setUpPieces();

	   //And now to make sure everyone has realized that the state has changed:
	   this.fireGameStateChanged(true);

	   //And now we have a nice visual effect:
	   new Effect.Parallel([
	       new Effect.Opacity(this.board, {from : 1, to : 0, duration : 0.5,
					       transition : Effect.Transitions.sinoidal,
					       sync : true}),
	       new Effect.Opacity(this.board, {from : 0, to : 1, duration : 0.5,
					       transition : Effect.Transitions.sinoidal,
					       sync : true})
	   ]);
       },

       //-------------------------Completely Non-interface stuff:-------------------------//
       /* Returns the current state of the game as a hash with string representing each piece as values
          and all the possible square positions as keys. Any key without a piece will contain null. */
       getGameState: function() {
	   var board = new Hash();
	   var square;//The current square.
	   var piece;//The current square's piece.

	   for (var i = 0, squares = this.squares, len = squares.length; i < len; ++i) {
	       square = squares[i];
	       piece = square.piece;
	       piece = piece ? piece.toString() : null;//Changes the piece to its representation for the hash.
	       board.set(square.id, piece);
	   }

	   //We have to account for en passant here:
	   if (this.enPassant) {//If it actually exists:
	       board.set(this.enPassant, "*");
	   }

	   return board;
       },
 
       /* Returns the old game state which is the game state before the last move was carried out. */
       getOldState : function () {
	   return this.oldState;
       }
   });

   /* This class encapsulates information about a change of game state. It contains the new game state as
      well as whether it is a new game (whether the change in state came about because of game.reset()). */
   var GameStateChangedEvent = Class.create({
       initialize: function(oldState, state, move, turn, newGame) {
	   /* Returns the game state before this change. */
	   this.getOldState = function () {
	       return oldState;
	   }

	   /* Returns the game state that is the result of the change. */
	   this.getState = function () {
	       return state;
	   }

	   /* Returns a boolean specifying whether the change in state is due to the game's being reset. */
	   this.isNewGame = function () {
	       return newGame ? true : false;
	   }

	   /*Returns the move, if any, that sparked this event. */
	   this.getMove = function () {
	       return move;
	   }

	   this.getTurn = function () {
	       return turn;
	   }
       }
   });
   
   var Sidebar = Class.create({
       initialize : function (options) {
	   if (!options) {
	       options = {};
	   }

	   //The actual DOM elment:
	   var className = options.className;
	   if (!className) {
	       className = "";
	   }
	   var element = new Element("div", {"class": className + " average sidebar"});
	   
	   //The two main parts of the sidebar:
	   var whitePortion = new Element("div", {"class" : "white portion"});
	   var blackPortion = new Element("div", {"class" : "black portion"});
	   
	   //The little tab that toggles this sidebar.
	   var tab = new Element("div", {"class" : "tab"});
	   var whiteCell = new Element("div", {"class" : "white cell"});
	   var blackCell = new Element("div", {"class" : "black cell"});

	   var hidden = true;//It is initially hidden from view.

	   var position = (typeof options.position == "number") ? options.position : 0;
	   var direction = options.direction == "right" ? "right" : "left";
	   var distance = (typeof options.distance == "number") ? options.distance : 0;

	   var bar;// The bar on the side of the tab with a little triangle on it.
	   if (options.direction != "right") {
	       bar = new Element("img", {"src" : "img/rightBar.png", "class" : "bar"});
	       bar.setStyle("float : right");
	   } else {
	       bar = new Element("img", {"src" : "img/leftBar.png", "class" : "bar"});
	   }

	   function toggle() {
	       var currX = element.getStyle(direction).replace(/\D/g, "") * 1;//Coerce into a number!
	       var dx = 0;//Distance to move horizontally.

	       if (hidden) {
		   dx = position + distance - currX;
	       }
	       
	       element.morph(direction + " : " + (position + dx) + "px");

	       if (bar.getAttribute("src").match(/.*left.*/)) {
		   bar.setAttribute("src", bar.getAttribute("src").replace("left", "right"));
	       } else {
		   bar.setAttribute("src", bar.getAttribute("src").replace("right", "left"));
	       }
				    

	       hidden = !hidden;//Toggle hidden.
	   };

	   var highlight = function () {
	       var curr = bar.getAttribute("src");
	       curr = curr.replace("img/", "img/highlighted");
	       console.log(curr);
	       bar.setAttribute("src", curr);
	   };
	   var dehighlight = function () {
	       var curr = bar.getAttribute("src");
	       curr = curr.replace("img/highlighted", "img/");
	       console.log(curr);
	       bar.setAttribute("src", curr);
	   }

	   element.insert(whitePortion);
	   element.insert(blackPortion);
	   element.insert(tab);

	   //Now we define the tab:
	   tab.insert(whiteCell);
	   tab.insert(blackCell);
	   tab.observe("click", toggle);
	   tab.observe("mouseover", highlight);
	   tab.observe("mouseout", dehighlight);

	   //Now we add the pertinent slider bar to the tab:
	   tab.insert(bar);

	   //Public properties:
	   this.element = element;
	   
	   this.whitePortion = whitePortion;
	   this.blackPortion = blackPortion;

	   this.tab = tab;
	   tab.whiteCell = whiteCell;
	   tab.blackCell = blackCell;
       }
   });

   /* This class represents a piece. It can be black or white and can be in a square. The
   piece's element is the div that contains it along with the appropriate image. */
   var Piece = Class.create({

       /* Creates and places the piece to the specified position, which is also its id. */
       initialize: function(type, color, position, parent) {
	   var selected = false;//Is this piece selected?

	   var initialPosition = position;//This one won't change; it's for reference.
	   var initialType = type;//This one shouldn't change either.

	   var picture;//This piece's picture (url).
	   var element;//This piece's actual element.
	   var img;//This piece's actual img tag.
	   var altText;//The img's alt-text.

	   //Position getter and setter:
	   this.getPosition = function () {
	       return position;
	   };

	   this.setPosition = function (newPosition) {
	       position = newPosition;

	       //If the piece was captured:
	       if (!position) {
		   parent.jail.addPiece(this);//Add this piece to the jail.
		   //It seems to not be dehighlighted immediately when moved to the jail, so...
		   this.dehighlight();
	       }
	   };

	   //Type getter and setter:
	   this.getType = function () {
	       return type;
	   };

	   /* If the given type is not valid, nothing will happen. */
	   this.setType = function (newType) {
	       if (["p", "R", "N", "B", "Q", "K"].indexOf(newType) < 0) {
		   return;
	       }
	       type = newType;
	       img.writeAttribute("src", "img/" + color.substring(0, 1) + type + ".png");
	   };

	   /* Returns the initial type of the piece. */
	   this.getInitialType = function () {
	       return initialType;
	   };

	   /* Returns the initial position of the piece. */
	   this.getInitialPosition = function () {
	       return initialPosition;
	   };

	   /* Toggles whether this piece is selected. */
	   this.select = function () {
	       this.setSelected(!selected);
	   };

	   /* Sets whether the piece is selected. */
	   this.setSelected = function (isSelected) {
	       selected = isSelected;

	       if (selected) {
		   this.element.setStyle({"background": "url(img/sl.png)"});
	       } else {
		   this.element.setStyle({"background": ""});
	       }
	   }

	   /* Returns whether the element is selected. */
	   this.isSelected = function () {
	       return selected;
	   }

	   //Actual DOM element stuff:
	   picture = "img/" + color.substring(0,1) + type + ".png";//The picture's URL.
	   //The substring is to account for the image's using single letter color names ("b" and "w").
	   element = new Element("div", {"class": "piece"}).update();
	   altText = color + " " + type;
	   img = new Element("img", {"src": picture, "alt": altText});
	   element.insert(img);
	   
	   //Observing the element for various interactions:
	   //NOTE: The one on "click" should only be observed if the side of this piece is on!
	   element.observe("mouseover", this.highlight.bind(this));
	   element.observe("mouseout", this.dehighlight.bind(this));

	   element.observe("click", function() { //Takes care of selecting the piece when clicked.
	       if (!parent.isSideOn(color)) {//If the side's off, just return here.
		   return;
	       }

	       //If it's this piece's turn and it is still on the board:
	       if (parent.turn == color && position) {
		   this.select();//Select the piece.
		   parent.setSelectedPiece(selected?this:null);//Notify the game of the selection.
		   if (!selected) {
		       this.highlight();//If it isn't selected, the mouse is over it, so it needs highlighting.
		   }
	       }
	   }.bind(this));


	   //To remove the piece from the game, permanently. It dies...
	   this.remove = function() {
	       if (position) {
		   parent.getSquare(position).setPiece(null);
	       }

	       try {
		   element.remove();
	       } catch(error) {
		   //This error does not actually stop the script; the method's not being called does nothing.
	       }
	   }

	   this.parent = parent;//The game which contains the piece.
	   this.color = color;//The pieces color ("black" or "white").
	   this.element = element;

	   //Placing the piece into a square:
	   parent.getSquare(position).setPiece(this);//Put it in its starting position.
       },

       /* Returns a string representation of the object with the first letter of the color followed by the type.
          For a black knight, it would return "bN". */
       toString: function() {
	   return this.color.substring(0, 1) + this.getType();
       },

       //------------------------------Highlighting, selecting and stuff:------------------------------//
       /* Highlights the pieces, as when somebody hovers their mouse over it. Does nothing if the piece is selected. */
       highlight: function() {
	   //This will do nothing if the piece is selected or not on the board.
	   if (!this.isSelected() && this.getPosition()) {
	       this.element.setStyle({"background": "url(img/hl.png)"});
	   }
       },

       /* Dehighlights the piece, as when the mouse leaves it. Does nothing if the piece is selected. */
       dehighlight: function() {
	   if (!this.isSelected()) {
	       this.element.setStyle({"background": ""});
	   }
       }
   });

   /* A square on the board with a color and a position */
   var Square = Class.create({

       /* Creates the square with the specified color and position as part of the specified game (parent) */
       initialize: function(color, position, parent) {
	   this.color = color;//The square's color ("white" or "black").
	   this.id = position;//The sqaure's id, which is its position (e.g. "a6")
	   this.parent = parent;//The game that contains the square.

	   this.highlighted = false;//Whether it is (non-actively) highlighted.
	   this.activeHighlighted = false;//Whether this is "active" highlighted.

	   //An observer for when the square needs to be actively selected. 
	   this.highlightObserver = this.activeHighlight.bind(this);
	   this.dehighlightObserver = this.dehighlight.bind(this);//An observer for when the square needs to be deselected.
	   this.moveObserver = this.moveSelectedPieceHere.bind(this);//An observer for moving the piece here when clicked.

	   //Actual DOM element stuff:
	   this.element = new Element("div", {"class": color + " square", "title" : position});

	   this.piece = null;//Any piece that may be in this square.

	   this.specialMove = null;//Any special move this square may be (in the future) doing.
       },

       /* This function sets this square's piece. If the square is empty--it contains no piece--then
          this method will set the piece, actually putting the piece into this square's div as well. If
          there is a piece already, this method (currently) does nothing.*/
       setPiece: function(piece) {
	   if (this.piece) {//To remove the current piece:
	       this.piece.element.remove();//Actually remove the piece from the document.
	   }

	   if (piece) {//If an actual piece needs its position updated:
	       if (!piece.getPosition()) {//Removes the piece from jail, if it is there:
		   try {
		       piece.element.remove();
		   } catch(error) {
		       //This error doesn't seem to be breaking anything when it occurs.
		   }
	       }

	       piece.setPosition(this.id);
	       this.element.insert(piece.element);//Actually places the piece.

	       //If a piece was here before:
	       if (this.piece) {
		   this.piece.setPosition(null);
	       }
	   }

	   this.piece = piece;//Finally, we update this.piece.
       },

       //------------------------------Highlighting, selecting and stuff:------------------------------//
       /* Highlights the square, chaning it's background image appropriately. This also start to watch
          the square for when it is hovered over, in order to actively select it in such a case. */
       highlight: function() {
	   this.highlighted = true;

	   this.element.setStyle({backgroundImage: "url(img/hls.png)", cursor: "pointer"});
	   this.element.observe("mouseover", this.highlightObserver);
       },

       /* Makes the square "actively" highlighted, as when it is mouseovered while highlighted. */
       activeHighlight: function() {
	   this.activeHighlighted = true;

	   this.element.setStyle({backgroundImage: "url(img/hl.png)", cursor: "pointer"});
	   this.element.observe("mouseout",  this.dehighlightObserver);
	   this.element.observe("click", this.moveObserver);
       },

       /* Insures that the square is not highlighted. This also deals with all the variables and observers. */
       dehighlight: function(completely) {
	   if (this.activeHighlighted) {//If it is actively highlighted:
	       this.activeHighlighted = false;
	       this.element.stopObserving("mouseout", this.dehighlightObserver);
	       this.element.stopObserving("click", this.moveObserver);
	       this.highlight();
	   } else if (this.highlighted) {//If it's just normally highlighted:
	       this.highlighted = false;
	       this.element.stopObserving("mouseover",this.highlightObserver );
	       this.element.setStyle({backgroundImage: "", cursor: "auto"});
	       this.specialMove = null;//Make sure this doesn't have some special move set...
	   }

	   if (completely==true) {//To make sure it's dehighlighted completely, no matter what:
	       this.dehighlight();
	   }
       },
 
       /* Moves the game's selected piece to this square */
       moveSelectedPieceHere: function() {
	   var move = this.parent.selectedPiece.getPosition() + "-" + this.id;//The move to make...
	   
	   if (this.specialMove) {
	       move = this.specialMove;
	   }

	   this.parent.makeMove(move);
       }
   });

   /* A chess clock that lets people play timed games locally or against the computer. Each time
      the turn starts, that side's time starts ticking away; the countdown is paused as soon as
      it is the other side's turn, and then that side starts losing time. This is just the model
      for a clock; it only keeps the time and sends out events when appropriate--it does not
      actually display the time anywhere. */
   var Clock = Class.create({
       /* Create the clock attached to the specified game. */
       initialize : function (game) {
	   var RESOLUTION = 1000;// How accurate the clock is.

	   // The listeners are alerted every second:
	   var listeners = [];

	   this.addListener = function (listener) {
	       listeners.push(listener);
	   }

	   /* Fires an event notifying all of the listeners that the time has changed. */
	   function timeChanged(reset) {
	       var event = {
		   //Was the clock reset to trigger this event?
		   reset : reset ? true : false,
		   turn : game.turn,
		   timeLeft : currTime[game.turn]
	       };

	       listeners.each(function (listener) {
		   try {
		       listener(event);
		   } catch (error) {
		       console.log(error);
		   }
	       });
	   }
	       

	   // Each side gets the default amount of time:
	   var currTime = {
	       black : this.turnTime,
	       white : this.turnTime
	   };

	   // The time left for this side at some time in the past.
	   var oldTime = new Date().getTime();
	   var oldTurnTime;
	   // Whose turn it was last time the time was updated.
	   var prevTurn = game.turn == "white" ? "black" : "white";

	   function updateTime() {
	       // If the turn has changed since the last update:
	       if (game.turn != prevTurn) {
		   prevTurn = game.turn;
		   oldTurnTime = 0;
	       }

	       var dTime = (new Date().getTime()) - oldTime;
	       oldTime = new Date().getTime();
	       currTime[game.turn] -= dTime;

	       if (!oldTurnTime) {
		   oldTurnTime = currTime[game.turn];
	       }

	       // If more than a second has passed, fire and event:
	       if (oldTurnTime - currTime[game.turn] >= 1000) {
		   timeChanged();
		   oldTurnTime = currTime[game.turn];
	       }
	   }

	   // DEBUG ONLY:
	   function reportTime(event) {
	       event.time *= 1;
	       var time = event.time / 60000;//In minutes.
	       time = Math.round(time, 0);
	       time += ":";
	       time += Math.round(event.time / 1000 % 60, 0);
	       
	       console.log("[clock] " + event.side + ": " + time);
	   }
	   this.addListener(reportTime);
	   
	   // Start the countdown immediately:
	   setInterval(updateTime, 100);
       },

       /* The default time allocated to each player, in milleseconds. */
       turnTime : 900000
   });      

//--------------------------------END OF CHESS INTERFACE CODE-------------------------------------//
//The following code just starts things:
   function onld() {
       game = new Game("holder");
   }