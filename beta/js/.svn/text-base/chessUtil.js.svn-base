/* This is a set of utilities for chess. The functions here do not have a persistent
   game state--it is passed as a first argument to any function that needs it--so they
   cannot account for certain state-based rules like not castling if the king or rook
   in question have moved. The game state is always assumed to be an object with a
   property for every square, so that gameState[square] tells us what sort of piece
   is at that position. The peice is represented by a simple two character string with
   the first character being the color ("b" or "w") and the second being the type ("p",
   "R", "N", "B", "Q" or "K"). The ChessUtil object is really just a namespace for the
   whole set of functions.*/

   var ChessUtil = {
       /* Returns whether the given position is valid. */
       isPositionValid : function (position) {
	   return /[a-h][1-8]/i.test(position);
       },

       /* Returns the specified move in algebraic notation. If the specified move is not valid, null will
          be returned. The gameState provided has to be the state BEFORE the move and is used to account
          for possible ambiguity.*/
       toAlgebraicNotation : function (gameState, move) {
	   var ambiguousFile = false;//Does ambiguity force this piece's file to be added?
	   var ambiguousRank = false;//Does ambiguity force this piece's rank to be added?
	   var piece;//The piece that moves in the given move.
	   var samePieces = [];//All the pieces with the same type and color on the board.
	   var toReturn = "";//The move to return.
	   
	   if (move == "0-0" || move == "0-0-0") {
	       return move;
	   }

	   //The move has to be validly formatted; if it isn't, we return null.
	   if (!/[a-h][1-8]\-[a-h][1-8].*/i.test(move)) {
	       return null;
	   }

	   move = move.split("-");
	   piece = ChessUtil.getPiece(gameState, move[0]);
	   if (!piece) {//If there is no piece, the given move is not valid.
	       return null;
	   }

	   if (piece.type != "p") {//Since pawns don't get a letter when using algebraic notation...
	       toReturn += piece.type;
	   }

	   //Now we need to check for possible ambiguity:
	   gameState.each(function (pair) {
	       var moves;//The moves a possibly ambiguous piece can make.
	       var posPiece;//A possibly ambiguous piece.

	       //We need to make sure it doesn't find itself as a possible ambiguity:
	       if (pair.key == piece.position) {
		   return;
	       }

	       if (pair.value === null) {//If there is no piece, we can skip this square.
		   return;
	       }

	       //If it's a piece of the same type and color, which could--but doesn't have to--be ambiguous.
	       posPiece = ChessUtil.getPiece(gameState, pair.key);
	       if (posPiece && (posPiece.type == piece.type) && (posPiece.color == piece.color)) {
		   moves = ChessUtil.possibleMoves(gameState, posPiece.position);
		   if (moves.indexOf(move[1]) >= 0) {//An ambiguity arises!
		       //If the file is sufficient to diffrentiate between the pieces:
		       if (posPiece.position.substring(0, 1) != move[0].substring(0, 1)) {
			   ambiguousFile = true;
		       } else {
			   //If the file isn't enough, then the rank is.
			   ambiguousRank = true;
		       }
		   }
	       }
	   });

	   //Now we get to deal with any ambiguity:
	   if (ambiguousFile) {
	       toReturn += move[0].substring(0, 1);
	   }
	   if (ambiguousRank) {
	       toReturn += move[0].substring(1);
	   }

	   //Next, we check for a possible capture, to be signified with an "x".
	   if (gameState.get(move[1])) {
	       toReturn += "x";
	   }

	   //Now we just append the destination square to the notation:
	   toReturn += move[1];

	   return toReturn;
       },

       /* Returns the next square from the given square in the current direction or, if a number is
          supplied, returns the square that many away from the current square in the specified direction.
          If the square is not valid, null is returned. The direction has to be "n", "s", "w", "e", "nw",
          "ne", "sw", "se". If it is not, null is returned. */
       nextPosition : function (position, direction, number) {
	   number = number ? number : 1;//Assume it's 1 if it's null or otherwise false. 
	   //Check whether the position is valid, returning null otherwise:
	   if (! /[ns]?[ew]?/.test(direction)) {
	       return null;
	   }

	   if (! ChessUtil.isPositionValid(position)) {//If the position is not valid:
	       return null;
	   }
	   position = position.split("");//Now it's an array! a6 => {'a', '6'}

	   var letters = $A($R("a", "h"));//The valid letters for the position.
	   
	   var col = letters.indexOf(position[0]);//The number corresponding to the current column.
	   
	   //Now for the actual moving:
	   if (direction.indexOf("n")>=0) {//Moving north (up)
	       position[1] *= 1;//Making sure this is treated as a number rather than a string.
	       position[1] += number;
	   }
	   
	   if (direction.indexOf("s")>=0) {//Moving south (down)
	       position[1] -= number;
	   }

	   if (direction.indexOf("w")>=0) {//Moving west (left)
	       col -= number;
	   }

	   if (direction.indexOf("e")>=0) {//Moving east (right):
	       col += number;
	   }

	   //Now check for the validity of the new position, returning null if invalid:
	   if (col < 0 || col >= 8 || position[1] <= 0 || position[1] > 8) {
	       return null;
	   }

	   //Now we return the position, put back together into a nice string:
	   col = letters[col];//Turning the column back into a letter
	   return col + position[1];//Returning the resulting position.
       },

       /* Checks whether the king of the specified color is in check in the given game state. */
       checkForCheck : function (gameState, color) {
	   var check = false;
	   color = color.substring(0, 1);//We only care about the color's first letter.

	   gameState.each(function(pair) {
	       //If the square does not contain a piece of the opposite color, skip:
	       if (!pair.value || pair.value.substring(0, 1)==color) {
		   return;
	       }

	       if (ChessUtil.possibleMoves(gameState, pair.key, {checkForCheck : true})) {
		   check = true;//If the king is in check, make this true.
	       }
	   });

	   return check;//Finally return whether the king is in check.
       },

       /* Returns whether the specified move in the specified game state results in check on the specified color. */
       checkMoveForCheck : function (gameState, move, color) {
	   gameState = ChessUtil.doMove(gameState, move);
	   return ChessUtil.checkForCheck(gameState, color);
       },

       /* Returns an array of booleans corresponding to whether each move in the moves array will lead to
          that color's king being in check. True means that that move will lead to check; false that it
          will not. */
       checkMovesForCheck : function (gameState, position, moves) {
	   var check = [];
	   var piece = ChessUtil.getPiece(gameState, position);

	   moves.each(function(move) {//Checking each of the possible moves for check:
	       move = piece.position + "-" + move;
	       check.push(ChessUtil.checkMoveForCheck(gameState, move, piece.color));//Add the result to the array.
	   });

	   return check;
       },

       /* Performs the move on the specified game state, returning the resulting state. The move should
          be in algebraic notation, with an optional color argument that is not optional if the move is
          something ambiguous, like castling. If the move, for some reason, is invalid, null is returned. */
       doMove : function (gameState, move, color) {
	   var state = gameState.clone();

	   move = move.replace(" ", "");//Gets rid of spaces.
	   if (move.length < 5) {//If the move is too short to be valid:
	       return null;
	   }
	   var startPos = move.substring(0, 2);//The starting position.
	   var endPos = move.substring(3, 5);//The ending position.

	   if (!(ChessUtil.isPositionValid(startPos) && ChessUtil.isPositionValid(endPos))) {//If either of the positions is invalid:
	       return null;
	   }

	   //And here is the actual move:
	   var piece = state.get(startPos);
	   state.set(startPos, null);
	   state.set(endPos, piece);

	   return state;//Return the resulting game state.
       },

       /* Returns whether the specified square in the given game state is attack by the color in question. */
       isSquareAttacked : function (gameState, square, color) {
	   var pieces = [];//All the pieces of that color on the board.
	   var checkedSquares = [];

	   gameState.each(function (pair) {
	       var piece = ChessUtil.getPiece(gameState, pair.key);
	       if (piece && piece.color == color.substring(0, 1)) {//If there is a piece of the specified color:
		   pieces.push(piece);
	       }
	   });

	   var attacked = false;//Was the square attacked?

	   pieces.each(function (piece) {//Check whether each piece attacks the square in question:
	       if (ChessUtil.possibleMoves(gameState, piece.position, {checkSquare : square})) {
		   attacked = true;
	       }
	   });

	   return attacked;//Finally, we return the result.
       },

       /* Returns a piece object based on the piece at the given position in the given game state. This
          will return null if there is no piece at the specified position. */
       getPiece : function (gameState, position) {
	   var piece = new Object();//This represents the piece for which to get moves.

	   var pieceString = gameState.get(position);
	   if (!pieceString) {
	       return null;//Return null if there is no piece at the specified location.
	   }

	   piece.color = pieceString.substring(0,1);//Get the piece's color
	   piece.type = pieceString.substring(1);//Get the piece's type.
	   piece.position = position;   
	   piece.toString = function() {
	       return this.color + this.type;
	   }
	   
	   return piece;
       },

       /* Gets the possible moves for the specified piece. By default, these are returned as an array. The function
          takes two arguments--a game state in the form of a hash as per Game#getGameState() and a position which
          has to be valid. If the position is not valid, null will be returned. If the game state is not valid,
          it'll probably throw an error. The two mandatory arguments can be followed by a list of options in the
          for of an object literal like "{checkForCheck : true}" or, of course, just an object with those properties
          defined as necessary. This list of options can change what the function does at a very fundamental level:
          they can change the returned type (only to boolean, currently) and can also change its time and memory
          requirements extensively.

          Only the following options are currently recognized by this function:
             checkForCheck : boolean -- if set to true, this will return whether the specified piece puts the enemy
                                        king in check.
             checkSquare : position -- checks whether the specified square is attacked by the piece. If checkForCheck
                                       is set to true, this is ignored.
             hasMoves : boolean -- sets whether to just check *if* there are any moves, without actually getting all
                                   of them. This returns a boolean and should be faster than getting the entire list
                                   of moves then checking the length. This one supercedes both checkForCheck and
                                  checkSquare.

          All the other ChessUtil move-finding functions accept these options, although some may accept more options.
    */
       possibleMoves : function (gameState, position, options) {
	   if (!ChessUtil.isPositionValid(position)) {
	       return null;//If the position is not valid, null is returned!
	   }

	   var moves = [];//The array of moves to return in the end.

	   if (!gameState.get(position)) {
	       return moves;//If there is no piece at the position, an empy array is returned.
	   }

	   var type = ChessUtil.getPiece(gameState, position).type;

	   //Now we find the moves depending on the type of piece we have:
	   switch(type){
	   case "p":
	       moves = ChessUtil.pawnMoves(gameState, position, options);
	       break;
	   case "R":
	       moves = ChessUtil.rookMoves(gameState, position, options);
	       break;
	   case "N":
	       moves = ChessUtil.knightMoves(gameState, position, options);
	       break;
	   case "B":
	       moves = ChessUtil.bishopMoves(gameState, position, options);
	       break;
	   case "Q":
	       moves = ChessUtil.queenMoves(gameState, position, options);
	       break;
	   case "K":
	       moves = ChessUtil.kingMoves(gameState, position, options);
	       break;
	   default:
	       moves = null;
	       break;
	   }

	   //Finally, we return the result:
	   return moves;
       },

       /* Returns an array of moves with all the moves that would result in check (and are therefore
          illegal removed. */
       cleanMoves : function (gameState, piece, moves) {
	   //First, we get rid of all moves that would result in check:
	   var checkArray = ChessUtil.checkMovesForCheck(gameState, piece.position, moves);

	   //Here we loop through all the moves seeing if any are not valid (due to check):
	   for (var i = 0, len = checkArray.length; i < len; ++i) {
	       if (checkArray[i]) {//If the move leads to check.
		   moves[i] = null;
	       }
	   }

	   moves = moves.compact();//Removes all the invalid moves set to null in the previous loop.
	   return moves;//Finally, we return the array of moves.
       },

       /* Returns all the possible moves for a pawn, given the pawn and a game state. If the piece is not
          in the game state, null is returned. Otherwise, an array of possible moves, in the form of
          algebraic coordinates, is returned. */
       pawnMoves : function (gameState, position, options) {
	   if (!gameState.get(position)) {
	       return null;//Returns null if the piece isn't on the board.
	   }

	   var piece = ChessUtil.getPiece(gameState, position);//Get a piece object.

	   var moves = [];//The array of moves to ultimately return.

	   var direction = piece.color=="w" ? "n" : "s";//The general direction the pawn will move in.

	   var letters = $A($R("a", "h"));//Column letters.
	   var col = letters.indexOf(piece.position.substring(0, 1)) * 1;//Converted to a number.
	   var row = piece.position.substring(1) * 1;//Converted to a number.

	   var posMove;//A possible move to consider.
	   var move;//posMove fleshed out into a full move (e.g. e4 to e2-e4).

	   //Variables to do with options:
	   var checkSquare;
	   var hasMoves = false;
	   var returnBoolean = false;

	   //Options:
	   if (options) {
	       checkSquare = options.checkSquare;
	       if (options.checkForCheck === true) {//Type checking! What fun.
		   checkSquare = gameState.index((piece.color=="w" ? "b" : "w") + "K");
	       }

	       if (ChessUtil.isPositionValid(checkSquare)) {
		   returnBoolean = true;
	       } else {
		   checkSquare = null;
	       }

	       hasMoves = options.hasMoves;
	       if (hasMoves === true) {
		   checkSquare = null;
		   returnBoolean = true;
	       } else {
		   hasMoves = false;//It is forced to be a boolean at this point.
	       }
	   }

	   //First, we account for the pawn's first double-move:
	   if (row==2 || row==7) {
	       posMove = ChessUtil.nextPosition(piece.position, direction, 2);
	       if (posMove && !gameState.get(posMove)) {//If the given move is on the board and an empty square
		   var opDir = direction=="n" ? "s" : "n";//Gets the opposite direction.
		   //If the square directly in front of the pawn is empy, this move is valid:
		   if (!gameState.get(ChessUtil.nextPosition(posMove, opDir, 1))) {
		       if (hasMoves) {
			   //It's only a valid move if it does not result in check;
			   move = piece.position + "-" + posMove;
			   if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
			       return true;
			   }
		       }

		       moves.push(posMove);
		   }
	       }
	   }
	   //Now, the move directly in front of the pawn:
	   posMove = ChessUtil.nextPosition(piece.position, direction, 1);
	   if (!gameState.get(posMove)) {//If the square is empty:
	       if (hasMoves) {
		   //It's only a valid move if it does not result in check;
		   move = piece.position + "-" + posMove;
		   if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
		       return true;
		   }
	       }

	       moves.push(posMove);
	   }

	   //First, we check for the square west and in front of the pawn:
	   posMove = ChessUtil.nextPosition(piece.position, direction + "w", 1);
	   //We can only take a piece of the opposite color, so:
	   if (gameState.get(posMove) && gameState.get(posMove).substring(0, 1) != piece.color) {
	       if (gameState.get(posMove).substring(1) != "K") {
		   if (hasMoves) {
		       //It's only a valid move if it does not result in check;
		       move = piece.position + "-" + posMove;
		       if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
			   return true;
		       }
		   }

		   moves.push(posMove);
	       }

	       if (posMove == checkSquare) {//If the pawn does attackt the square in question:
		   return true;
	       }
	   }

	   //Next, we check for the square east and in front of the pawn:
	   posMove = ChessUtil.nextPosition(piece.position, direction + "e", 1);
	   //We can only take a piece of the opposite color, so:
	   if (gameState.get(posMove) && gameState.get(posMove).substring(0, 1) != piece.color) {
	       if (gameState.get(posMove).substring(1) != "K") {
		   if (hasMoves) {
		       //It's only a valid move if it does not result in check;
		       move = piece.position + "-" + posMove;
		       if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
			   return true;
		       }

		   }

		   moves.push(posMove);
	       }

	       if (posMove == checkSquare) {//If the pawn does attackt the square in question:
		   return true;
	       }
	   }

	   //We need to make sure we're returning the proper thing:
	   if (returnBoolean) {
	       return false;//We don't care about the actual moves.
	   } else {
	       return ChessUtil.cleanMoves(gameState, piece, moves);
	   }
       },

       /* Returns all the possible moves for the specified knight. Otherwise is the same as ChessUtil.pawnMoves(). */
        knightMoves : function (gameState, position, options) {
	   if (!gameState.get(position)) {
	       return null;//Returns null if the piece isn't in the game state.
	   }

	   var piece = ChessUtil.getPiece(gameState, position);//Get a piece object.

	   var dirs = ["n", "s", "w", "e"];//The four cardinal directions.
	   var moves = [];//The possible moves

	   var posMove;//A possible move to check.
	   var move;//A move in full (that is, with two positions).

	   //Variables to do with options:
	   var checkSquare;
	   var hasMoves = false;
	   var returnBoolean = false;

	   //Options:
	   if (options) {
	       checkSquare = options.checkSquare;
	       if (options.checkForCheck === true) {//Type checking! What fun.
		   checkSquare = gameState.index((piece.color=="w" ? "b" : "w") + "K");
	       }

	       if (ChessUtil.isPositionValid(checkSquare)) {
		   returnBoolean = true;
	       } else {
		   checkSquare = null;
	       }

	       hasMoves = options.hasMoves;
	       if (hasMoves === true) {
		   checkSquare = null;
		   returnBoolean = true;
	       } else {
		   hasMoves = false;//It is forced to be a boolean at this point.
	       }
	   }

	   /* Returns either null (do nothing), a move (push it to the array) or a boolean (return it). */
	   function check() {
	       var posCap;//A possible piece to capture.

	       if (checkSquare && (posMove == checkSquare)) {//If it's the move we're checking for
		   return true;
	       } else if (checkSquare) {
		   return null;
	       }

	       if (posMove) {
		   posCap = gameState.get(posMove);//A piece that could possible be captured.

		   if (!posCap || posCap.substring(0, 1) != piece.color) {
		       if (posCap && posCap.substring(1)!="K") {//For a capturable piece:
			   if (hasMoves) {
			       move = piece.position + "-" + posMove;
			       if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
				   return true;
			       } else {
				   return null;
			       }
			   }
			   
			   return posMove;
		       } else if (hasMoves) {//Nothing to capture:
			   move = piece.position + "-" + posMove;
			   if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
			       return true;
			   } else {
			       return null;
			   }
		       }

		       return posMove;
		   }
	       }
	   }
	   
	   //Here we get check whether all the knight's moves are possible:
	   for (var i = 0; i < 2; i++) {
	       for (var j = 2; j < 4; j++) {
		   //The first of the possible L shapes:
		   posMove = ChessUtil.nextPosition(piece.position, dirs[i], 2);
		   posMove = posMove ? ChessUtil.nextPosition(posMove, dirs[j], 1) : null;

		   var result = check(posMove);
		   if (typeof result == "boolean") {//A boolean result to be return immediately.
		       return result;
		   } else if (result) {//A move to push on the array:
		       moves.push(result);
		   }

		   //The second of the possible L shapes:
		   posMove = ChessUtil.nextPosition(piece.position, dirs[i], 1);
		   posMove = posMove ? ChessUtil.nextPosition(posMove, dirs[j], 2) : null;

		   var result = check(posMove);
		   if (typeof result == "boolean") {//A boolean result to be return immediately.
		       return result;
		   } else if (result) {//A move to push on the array:
		       moves.push(result);
		   }
	       }
	   }

	   //We need to make sure we're returning the proper thing:
	   if (returnBoolean) {
	       return false;//We don't care about the actual moves.
	   } else {
	       return ChessUtil.cleanMoves(gameState, piece, moves);
	   }
       },

       /* Gets the moves for a piece that moves in a line, like a rook, bishop or queen. This function has the
          same arguments as any other, but accepts more options than the others. These new options are directions,
          an array of directions that must all be valid, and limit, the maximum distance the piece can go in a
          straight line. For most pieces a limit of 8 is fine; the king, obviously, has a limit of 1. Limit is 8
          by default, but directions _must_ be specified. */
       linearPieceMoves : function (gameState, position, options) {
	   if (!gameState.get(position)) {
	       return null;//Returns null if the piece isn't in the game state.
	   }

	   var piece = ChessUtil.getPiece(gameState, position);

	   //Variables to do with options:
	   var checkSquare;
	   var hasMoves = false;
	   var returnBoolean = false;

	   //Options:
	   if (options) {
	       checkSquare = options.checkSquare;
	       if (options.checkForCheck === true) {//Type checking! What fun.
		   checkSquare = gameState.index((piece.color=="w" ? "b" : "w") + "K");
	       }

	       if (ChessUtil.isPositionValid(checkSquare)) {
		   returnBoolean = true;
	       } else {
		   checkSquare = null;
	       }

	       hasMoves = options.hasMoves;
	       if (hasMoves === true) {
		   checkSquare = null;
		   returnBoolean = true;
	       } else {
		   hasMoves = false;//It is forced to be a boolean at this point.
	       }
	   }

	   var moves = [];//The possible moves.

	   var limit = options.limit * 1;
	   if (isNaN(limit)) {//Default value for limit, as promised.
               limit = 8;
	   }

	   var directions = options.directions;
	   
	   //Now we loop through the directions, getting the maximum amount of moves in each:
	   for (var i = 0, len = directions.length; i < len; ++i) {
	       var direction = directions[i];

	       var posMove = ChessUtil.nextPosition(position, direction, 1);
	       var numMoves = 0;//The number of moves done so far: it has to be smaller than limit.

	       while (posMove) {//This way we'll stop if we run out of squares:
		   var posCap = gameState.get(posMove);//A possible piece to capture.
		   //Now we make it into a convenient piece object:
		   posCap = posCap ? ChessUtil.getPiece(gameState, posMove) : null;

		   if (posMove == checkSquare) {//If this is the square we're checking:
		       return true;
		   }

		   if (!posCap) {//If the square is empty:
		       if (hasMoves) {
			   move = piece.position + "-" + posMove;
			   if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
			       return true;
			   }
		       }

		       if (!returnBoolean) {
			   moves.push(posMove);//Add the possible move.
		       }
		   } else if (posCap.color == piece.color) {//A friendly piece blocks the way...
		       break;//This direction is done then.
		   } else if (posCap.type != "K") {//If it's a capturable piece:
		       if (hasMoves) {
			   move = piece.position + "-" + posMove;
			   if (!ChessUtil.checkMoveForCheck(gameState, move, piece.color)) {
			       return true;
			   }
		       }

		       if (!returnBoolean) {
			   moves.push(posMove);
		       }
		       break;//Can't go any farther.
		   }

		   numMoves++;//Incrememnt the number of moves done.
		   if (numMoves >= limit) {//If it's time to stop:
		       break;
		   }

		   posMove = ChessUtil.nextPosition(posMove, direction, 1);
	       }
	   }

	   //We need to make sure we're returning the proper thing:
	   if (returnBoolean) {
	       return false;//We don't care about the actual moves.
	   } else {
	       return ChessUtil.cleanMoves(gameState, piece, moves);
	   }

       },

       /* Gets the moves for the specified bishop; works just like any of the other, similar functions. */
       bishopMoves : function (gameState, position, options) {
	   if (!options) {//Options has to be defined from now on:
	       options = {}
	   }

	   options.directions = ["nw", "ne", "sw", "se"];
	   options.limit = 8;

	   return ChessUtil.linearPieceMoves(gameState, position, options);
       },

       /* Gets the moves for the specified rook; works just like any of the other, similar functions. */
       rookMoves : function (gameState, position, options) {
	   if (!options) {//Options has to be defined from now on:
	       options = {}
	   }

	   options.directions = ["n", "s", "w", "e"];
	   options.limit = 8;

	   return ChessUtil.linearPieceMoves(gameState, position, options);
       },

       /* Gets the moves for the specified queen; works just like any of the other, similar functions. */
       queenMoves : function (gameState, position, options) {
	   if (!options) {//Options has to be defined from now on:
	       options = {}
	   }

	   options.directions = ["n", "s", "w", "e", "nw", "ne", "sw", "se"];
	   options.limit = 8;

	   return ChessUtil.linearPieceMoves(gameState, position, options);
       },

       /* Gets the moves for the specified king; works just like any of the other, similar functions, but
          also accounts for the possibility of castling, albiet non-deterministically--due to its not
          having any previous state information, it cannot check whether the king or rook in question
          have moved before; as such, it is up to the game itself to ultimately decide if castling is
          legal. */
       kingMoves : function (gameState, position, options) {
	   if (!options) {//Options has to be defined from now on:
	       options = {}
	   }

	   options.directions = ["n", "s", "w", "e", "nw", "ne", "sw", "se"];
	   options.limit = 1;

	   var moves = ChessUtil.linearPieceMoves(gameState, position, options);
	   if (typeof moves == "boolean") {//Castling can never be the only move, nor is it an attack:
	       return moves;
	   }

	   //Now that we have the linear moves, we check for castling:
	   //NOTE: This method has no previous state information and is thus not always accurate regarding castling!
	   var piece = ChessUtil.getPiece(gameState, position);
	   
	   var rank = piece.position.substring(1);//The king's rank, either 1 or 8 for castling.

	   var inCheck = ChessUtil.checkForCheck(gameState, piece.color);//Can't castle out of check.
	   var kingSide = true;
	   var queenSide = true;//The two sides.

	   /* Returns whether the specified file's home-row square is attacked. */
	   function checkFile(file) {//NO ARGUMENT VALIDATION!
	       var enemy = (piece.color == "w") ? "b" : "w";
	       return ChessUtil.isSquareAttacked(gameState, file + rank, enemy);
	   }

	   //The king can't travel through check to get to his new position after castling; this requires
	   //the checking of two sperate squares, first for attacks then for pieces.
	   kingSide = kingSide && !checkFile("f");
	   kingSide = kingSide && !checkFile("g");
	   kingSide = kingSide && !gameState.get("g" + rank);
	   kingSide = kingSide && !gameState.get("f" + rank);

	   if (kingSide && !inCheck) {
	       moves.push("0-0");
	   }

	   //And for queen-side:
	   queenSide = queenSide && !checkFile("c");
	   queenSide = queenSide && !checkFile("d");
	   queenSide = queenSide && !gameState.get("b" + rank);
	   queenSide = queenSide && !gameState.get("c" + rank);
	   queenSide = queenSide && !gameState.get("d" + rank);

	   if (queenSide && !inCheck) {
	       moves.push("0-0-0");
	   }

	   return moves;//Finally, we return the moves.
       }

   };