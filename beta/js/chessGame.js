var DEBUG = false;

/* The following bit is for debugging: */
if (!DEBUG) {
    var console = {};
} else {
    var game = new ChessGameModel();
    log(game.getMoves("b1"));
}

function log() {
    if (DEBUG) {
        var args = [];
        args.push("[Chess Game]:");
        
        for (var i = 0; i < arguments.length; i++) {
            args.push(arguments[i]);
        }

        console.log.apply(console, args);
    }
}

/* The following function makes life much easier when using arrays: */
function indexOf(element, array) {
    for (var i = 0; i < array.length; i++) {
        if (array[i] === element) {
            return i;
        }
    }

    return -1;
}

/* This is the model for a chess game. It keeps track of the game state, the
 * history of moves and can do rule-enforcement.
 */
function ChessGameModel() {

    var pieceTypes = ["p", "R", "N", "B", "Q", "K"],
        pieceTypeNames = ["pawn", "rook", "knight", "bishop", "queen", "king"],
        ranks = [1, 2, 3, 4, 5, 6, 7, 8],
        files = ["a", "b", "c", "d", "e", "f", "g", "h"],
        homeRow = ["R", "N", "B", "Q", "K", "B", "N", "R"];// Starting order

    var pieces = PieceList();// All of the piece currently on the board.
    var capturedPieces = [];// Captured pieces go here.

    var canCastle = {
        whiteQueenside : true,
        blackQueenside : true,
        whiteKingside : true,
        whiteQueenside : true
    };

    var history = [];// The moves in the game so far.

    var observers = [];// All functions to be called on game changes.

    var turn = "w";

    var enforceRules = true;

    /* Returns whether the given position is valid (e.g. it is inside the chess 
     * board in the right format). Positions are case-insensitive.
     */
    function isPositionValid(position) {
        if (!position) {
            return false;
        }

        position += "";
        return /^[a-h][1-8]$/i.test(position);
    }

    // Create all of the default pieces:
    for (var i = 0; i < 8; i++) {
        // White pieces:
        pieces.push(new Piece(files[i] + 1, homeRow[i], "w"));
        pieces.push(new Piece(files[i] + 2, "p", "w"));

        // Black pieces:
        pieces.push(new Piece(files[i] + 8, homeRow[i], "b"));
        pieces.push(new Piece(files[i] + 7, "p", "b"));	
    }

    /* Adds an observe to this game model. The observer will be called each time
     * the game changes in some way and passed a GameChangeEvent.
     */
    this.observe = function (observer) {
        observers.push(observer);
    };

    /* Removes the specified observer and returns it. If the observer is not
     * found, it is still returned.
     */
    this.removeObserver = function(observer) {
        for (var i = 0; i < observers.length; i++) {
            if (observer == observers[i]) {
                delete observers[i];
            }
        }

        return observer;
    };

    /* Fires the given event, passing a copy to all of the observers. */
    function fire(event) {
        for (var i = 0; i < observers.length; i++) {
            try {
                observers[i](new GameChangeEvent(event));
            } catch (x) {
                console.error(x);
                log("Could not call observer:", observers[i]);
                log("Removing this observer.");
                observers.splice(i, 1);
            }
        }

        /* Creates an event based on the passed object. */
        function GameChangeEvent(object) {
            object = object || {};
            
            /* Returns whose turn it is immediately after this event. */
            this.getTurn = function () {
                return turn;
            };

            /* Returns whether this event represnts a new game. */
            this.isNewGame = function () {
                return !!object.isNewGame;
            };

            /* Returns the move that prompted this event, if any: */
            this.getMove = function () {
                return object.move;
            };

            /* Returns the piece that was captured, if any: */
            this.getCapturedPiece = function () {
                return object.capturedPiece;
            };

            /* Returns whether any side has won the game. If white has won, then
             * "w" will be returned; if black has won then "b" will be returned;
             * if it is a stalemate, then "s" will be returned; if the game is
             * not over, then null will be returned here.
             */
            this.getWinner = function () {
                return object.winner;
            };

            /* Returns a convenient string representation of the object. */
            this.toString = function () {
                if (this.isNewGame()) {
                    return "Game changed: starting new game!";
                }
                return "Game changed; Move: " + this.getMove()
                    + "; piece captured: " + this.getCapturedPiece();
            };

        }
    }

    /* Gets the piece at the specified position, if any. If there is
     * no piece there, null is returned.
     */
    this.getPiece = function (position) {
        position = position.toLowerCase();

        return pieces.get(position);
    };

    /* Returns a new list of all of the pieces currently on the board. This list
     * cannot be used to change the game state; it is a new instance that has all
     * of the same pieces as this game.
     */
    this.getPieces = function () {
        return pieces;
    };

    /* Returns the current turn. This is just the color who can go, not the 
     * number of the turn or anything like that.
     */
    this.getTurn = function () {
        return turn;
    };

    /* If given a string, this will try to parse it as a position and return the
     * moves of the piece at the given position. If given a piece, this will get
     * the moves of that piece. In either case, if it is not the piece's turn, a
     * null list will be returned.
     */
    this.getMoves = function (piece) {
        if (typeof piece == "string") {
            if (!isPositionValid(piece)) {
                throw "The position given to getMoves is not valid. (" + piece 
                    + ").";
            } else {
                piece = this.getPiece(piece);
                if (!piece) {
                    return [];
                }
            }
        }

        if (piece.getColor() != this.getTurn()) {
            return [];
        }

        return pieceMoves(piece);
    };

    /* Sets whether the rules of chess are enforced. */
    this.setRulesEnforcement = function (enforcement) {
        enforceRules = enforcement;
    };

    /* Returns whether the rules of chess are enforced. */
    this.getRulesEnforcement = function () {
        return enforceRules;
    };
    
    /* Does the specified move. If the move is not legal, it may not be done.
     * Some moves (like those involving pieces that don't exist or invalid
     * positions) will never be done; others will not be done only if rules
     * enforcement is on. If the move is not completed, false will be returned;
     * otherwise true is returned. 
     */
    this.move = function (move) {
        var piece;
        
        if (/^0-0(-0)$/.test(move)) {
            piece = this.getPiece(getEnemyKingPosition(
                this.getTurn() == "w" ? "b" : "w"));
        } else {
            if (!/^[a-h][1-8]-[a-h][1-8](\*[pRNBQK])?$/.test(move)) {
                log("The given move", move, "is not valid!");
            } else {
                log("Move:", move);
            }
            piece = this.getPiece(move.split("-")[0]);

            if (!piece) {
                return false;
            }

            move = move.split("-")[1];
        }
        
        if (enforceRules && indexOf(move, this.getMoves(piece)) < 0) {
            return false;
        }

        var moveMade = null;
        var previousPosition = piece.getPosition();

        var king, rank, rook;
        
        if (move == "0-0") {
            king = piece;
            rank = (king.getColor() == "w") ? 1 : 8;
            rook = pieces.get("h" + rank);

            pieces.set("g" + rank, king);
            pieces.set("f" + rank, rook);

            moveMade = "0-0";
        } else if (move == "0-0-0") {
            king = piece;
            rank = (king.getColor() == "w") ? 1 : 8;
            rook = pieces.get("a" + rank);

            pieces.set("c" + rank, king);
            pieces.set("d" + rank, rook);

            moveMade = "0-0-0";
        } else {
            var removedPiece = pieces.set(move, piece);
            
            if (removedPiece) {
                capturedPieces.push(removedPiece);
            }

            moveMade = previousPosition + "-" + move;
        }

        turn = (turn == "w") ? "b" : "w";
        if (!hasMoves()) {// The game is now over!
            if (checkForCheck(turn)) {
                fire({winner : (turn == "w") ? "b" : "w"});
            } else {
                fire({winner : "s"});
            }
        } else {
            fire({move : moveMade, capturedPiece : removedPiece});
        }
        
        return true;
    };

    /* Returns whether the given color can castle in the given way. */
    function canColorCastle(color) {
        return canCastle[color];
    };

    //Rule enforcement:

    /* Gets all of the legal moves for the specified piece. If the piece is not 
     * on the board, an error is thrown. The options object controls how the
     * moves are found and what is returned. In particular, there are several
     * options:
     *         checkForCheck : boolean - if true, this will return whether the
     *                                   specified piece puts the enemy king in
     *                                   check.
     *         checkSquare : position -  if set, returns whether the specified
     *                                   square is attacked by the piece. If
     *                                   checkForCheck is true, this is ignored.
     *         hasMoves : boolean -      sets whether to just check for the
     *                                   existence of moves. This is faster and
     *                                   more efficient than getting a list as it
     *                                   quits as soon as it finds a move and
     *                                   does not save any moves. This supercedes
     *                                   both checkForCheck and checkSquare.
     * All of the moves functions accept these options, but some may also accept
     * additional options.
     */
    function pieceMoves(piece, options, state) {
        state = state || pieces;

        if (!piece || !state.contains(piece)) {
            throw "The specified piece does not exist (" + piece + ").";
        }

        // Dealing with the options:
        options = options || {};

        if (options) {
            var checkSquare = options.checkSquare;
            var returnBoolean = false;

            if (options.checkForCheck === true) {
                checkSquare = getEnemyKingPosition(piece.getColor());
            }

            if (isPositionValid(checkSquare)) {
                returnBoolean = true;
            } else {
                checkSquare = null;
            }

            var hasMoves = options.hasMoves;
            if (hasMoves === true) {
                checkSquare = null;
                returnBoolean = true;
            } else {
                hasMoves = false;//It is forced to be a boolean at this point.
            }
        }
        
        var color = piece.getColor();
        var col = indexOf(piece.getPosition().substring(0, 1) * 1, ranks);
        var row = piece.getPosition().substring(1) * 1;

        var posMove;// A possible square to move to.
        var move;// Generally a full move to check.

        /* Returns a function that gets the moves for a piece that moves in a
         * straight line in the given directions with a maximum defined by 
         * limit, which is 8 by default. The returned function works much the
         * same way as all of the other move-finding functions.
         */
        function linearPieceMoves(directions, limit) {
            var moves = [];
            limit = isNaN(limit) ? 8 : limit;
            if (!directions || directions.length < 1) {
                return function () { return null;};
            }

            return function () {
                // Get the moves for all of the directions given:
                for (var i = 0; i < directions.length; i++) {
                    var direction = directions[i];
                    posMove = nextPosition(piece.getPosition(), direction, 
                                           1);
                    var numMoves = 0;//The number of moves in this direction.

                    while (isPositionValid(posMove) && numMoves < limit) {
                        if (checkSquare && posMove == checkSquare) {
                            return true;
                        }

                        if (state.get(posMove)) {
                            if (canCapture(piece.getColor(), 
                                           posMove, state)) {
                                if (hasMoves) {
                                    move = piece.getPosition() + "-" 
                                        + posMove;
                                    if (!checkMoveForCheck(piece.getColor(), 
                                                           move)) {
                                        return true;// The piece has a move.
                                    }
                                } else if (!returnBoolean) {
                                    moves.push(posMove);
                                }
                            } 

                            break;
                        } else {
                            if (hasMoves) {
                                move = piece.getPosition() + "-" 
                                    +  posMove;
                                if (!checkMoveForCheck(piece.getColor(), 
                                                       move)) {
                                    return true;// The piece has a move.
                                }
                            } else if (!returnBoolean) {
                                moves.push(posMove);
                            }
                        }

                        posMove = nextPosition(posMove, direction, 1);
                        numMoves++;
                    }
                }

                return returnBoolean ? false : moves;
            };
        }

        var findMoves = {
            /* Returns all of the legal moves for the given pawn. The options are
             * the same as for pieceMoves.
             */
            p : function () {
                var direction = (piece.getColor() == "w") ? "n" : "s";
                var moves = [];

                //Now, the move directly in front of the pawn:
                posMove = nextPosition(piece.getPosition(), direction, 1);
                if (!state.get(posMove)) {
                    moves.push(posMove);

                    //If that was legal, maybe hopping up two is legal too:
                    if (row == 2 || row == 7) {
                        posMove = nextPosition(piece.getPosition(), 
                                               direction, 2);

                        if (!state.get(posMove)) {				
                            moves.push(posMove);
                        }
                    }
                }

                /* Checks whether the pawn can capture in the specified 
                 * direction. */
                function posCap(side) {
                    posMove = nextPosition(piece.getPosition(), direction + side,
                                           1);

                    if (canCapture(piece.getColor(), posMove)) {
                        moves.push(posMove);
                    }
                }

                posCap("w");
                posCap("e");

                for (var i = 0; i < moves.length; i++) {
                    if (moves[i] == checkSquare) {
                        return true;
                    } else if (hasMoves) {
                        move = piece.getPosition() + moves[i];
                        if (!checkMoveForCheck(piece.getColor(), move)) {
                            return true;
                        }
                    }
                }

                return returnBoolean ? false : moves;
            },

            N : function () {
                var dirs = ["n", "s", "w", "e"];// The four cardinal directions.
                var moves = [];

                /* Returns what to do next: null--do nothing; a move--push it; 
                 * boolean--return it.
                 */
                function check(posMove) {
                    // If we've found the move we're checking for:
                    if (checkSquare) {
                        return (posMove == checkSquare) ? true : null;
                    }

                    if (posMove) {
                        move = piece.getPosition() + "-" + posMove;

                        if (canCapture(piece.getColor(), posMove, state)) {
                            if (hasMoves) {
                                if (checkMoveForCheck(piece.getColor(), move)) {
                                    return true;
                                } else {
                                    return null;
                                }
                            }

                            return posMove;
                        } else if (!state.get(posMove)) {
                            if (hasMoves) {
                                if (!checkMoveForCheck(piece.getColor(), move)) {
                                    return true;
                                } else {
                                    return null;
                                }
                            }

                            return posMove;
                        }
                    }

                    return null;
                }
                // Now we check each of the knight's possible moves:
                for (var i = 0; i < 2; i++) {
                    // We cover two L shapes in each iteration of the inner loop.
                    for (var j = 2; j < 4; j++) {
                        posMove = nextPosition(piece.getPosition(), dirs[i], 2);
                        posMove = posMove
                            ? nextPosition(posMove, dirs[j], 1) 
                            : null;
                        
                        var result = check(posMove);

                        if (typeof result == "boolean") {
                            return result;
                        } else if (result) {
                            moves.push(result);
                        }

                        posMove = nextPosition(piece.getPosition(), dirs[i], 1);
                        posMove = posMove 
                            ? nextPosition(posMove, dirs[j], 2) 
                            : null;

                        result = check(posMove);
                        if (typeof result == "boolean") {
                            return result;
                        } else if (result) {
                            moves.push(result);
                        }
                    }
                }

                return returnBoolean ? false : moves;
            },

            // The next three functions get the moves for their respective piece.
            B : linearPieceMoves(["sw", "nw", "ne", "se"]),

            R : linearPieceMoves(["n", "s", "w", "e"]),

            Q : linearPieceMoves(["sw", "nw", "ne", "se", "n", "s", "w", "e"]),

            K : function () {
                var moves = linearPieceMoves(["sw", "nw", "ne", "se", "n", "s",
                                              "w", "e"], 1)();
                if (typeof moves == "boolean") {
                    return moves;// Castling cannot be the only move or an attack
                }

                // Castling:
                if (canCastle[piece.getColor() + "Queenside"]) {
                    moves.push("0-0-0");
                }

                if (canCastle[piece.getColor() + "Kingside"]) {
                    moves.push("0-0");
                }

                return moves;
            }
        };

        result = findMoves[piece.getType()]();

        return (typeof result == "boolean")
            ? result
            : cleanMoves(piece, result);
    }

    //Utility functions:

    /* Finds the position that is directly in line with the given position. The
     * direction should be specified as a cardinal direction (n, s, w, e, nw, ne,
     * sw and se). The number specifies how far to go in the specified direction.
     * If any of the given parameters or the position that results are not valid,
     * null will be returned.
     */
    function nextPosition(position, direction, number) {
        if (!direction || !/^[ns]?[ew]?$/.test(direction)
            || !isPositionValid(position)) {
            return null;
        }
        
        number = number || 1;

        // Convert the position to an array of two nubmers:
        position = position.split("");
        var letter = position[0] + "";
        position[0] = indexOf(position[0], files);

        var temp = [position[0], position[1]];
        var dirs = [false, false, false, false];

        //Now for the actual moving:
        if (/n/.test(direction)) {// Moving north (up)
            position[1] *= 1;
            position[1] += number;
            dirs[0] = true;
        }

        if (/s/.test(direction)) {// Moving south (down)
            position[1] -= number;
            dirs[1] = true;
        }

        if (/w/.test(direction)) {// Moving west (left)
            position[0] -= number;
            dirs[2] = true;
        }

        if (/e/.test(direction)) {// Moving east (right)
            position[0] += number;
            dirs[3] = true;
        }

        // Now we turn the resulting two numbers into a valid position:
        var pos = files[position[0]] + "" + position[1];
        return isPositionValid(pos) ? pos : null;
    }

    /* Returns whether a piece of the specified color can capture the piece at 
     * the given position. By default, uses the current list of pieces, but a 
     * different list can be passed as well. If there is no piece at the given
     * location, returns false.
     */
    function canCapture(color, pos, state) {
        state = state || pieces;
        var piece = state.get(pos);

        if (!piece) {
            return false;
        }

        return piece.getColor() != color && piece.getType() != "K";
    }

    /* Returns an array of pieces that corresponds to the state that would result
     * after the application of the given move on a state. If the state is not
     * given, then the move is done on the current state. The state should be a
     * PieceList.
     */
    function doMove(move, state) {
        state = state || pieces;

        var pieceList = PieceList(state);

        var start = move.substring(0, 2);
        var end = move.substring(3);

        if (!(isPositionValid(start) || isPositionValid(end))) {
            throw "Move passed to doMove not valid (" + move + ")!";
        }

        if (!state.get(start)) {
            throw "No piece to move at start of " + move + "!";
        }

        pieceList.set(end, pieceList.get(start));
        pieceList.removeAt(start);

        return pieceList;
    }

    /* Returns whether the given move is valid. This does not gauge whether the
     * move is legal, but just whether it is formatted properly.
     */
    function isMoveValid(move) {
        if (!move) {
            return false;
        }
        move += "";
        move = move.split("-");
        return isPositionValid(move[0]) && isPositionValid(move[1]);
    }

    /* Checks whether the king of the given color is in check with the given set
     * of pieces. If the pieces are not provided, the current pieces are used.
     */
    function checkForCheck(color, pieceList) {
        var check = false;
        pieceList = pieceList || pieces;	
        
        // Walk through the pieces and see if any of them attack the king.
        for (var i = 0; i < pieceList.length; i++) {
            if (pieceList[i].getColor() != color) {
                if (pieceMoves(pieceList[i], {checkForCheck : true})) {
                    check = true;
                    break;
                }
            }
        }

        return check;
    }

    /* Checks whether the given move results in check against the specified 
     * color.
     */
    function checkMoveForCheck(color, move) {
        // Note: only checks moves from current state.
        return checkForCheck(color, doMove(move));
    }

    /* Returns the given list of moves with all of the moves resulting in check
     * removed.
     */
    function cleanMoves(piece, moves) {
        var color = piece.getColor();

        for (var i = 0; i < moves.length; i++) {
            var move = piece.getPosition() + "-" + moves[i];
            if (!isPositionValid(moves[i])
                || checkForCheck(color, doMove(move))) {
                moves.splice(i, 1);
                i--;
            }
        }

        return moves;
    }

    /* Returns the position of the king who is not of the specified color. If the
     * king cannot be found, an error is thrown.
     */
    function getEnemyKingPosition(color) {
        for (var i = 0; i < pieces.length; i++) {
            if (pieces[i].getColor() != color && pieces[i].getType() == "K") {
                return pieces[i].getPosition();
            }
        }

        throw "King not found by getEnemyKingPosition!";
    }

    /* Returns whether the given side has any legal moves. If no side is supplied
     * then this will take the side whose turn it is currently.
     */
    function hasMoves(side) {
        side = side || turn;

        var hasMoves = false;
        pieces.each(function (piece) {
            if (piece.getColor() == side) {
                hasMoves = hasMoves || pieceMoves(piece, {hasMoves : true});
            }
        });

        return hasMoves;
    }

    /* Returns the given move in proper algebraic notation. */
    function toAlgebraicNotation(move, state) {
        state = state || pieces;
        var inAlgebraicNotation = "";

        if (move.match(/^0-0(-0)?$/)) {
            return move;
        } else if (!/[a-h][1-8]-[a-h][1-8]/.test(move)) {
            log("Invalid move; cannot get algebraic representation: ", move);
            return null;
        }

        move = move.split("-");

        var piece = state.get(move[0]);
        if (!piece) {
            return null;
        }

        if (piece.getType() != "p") {
            inAlgebraicNotation += piece.getType();
        }

        var color = piece.getColor(),
            type = piece.getType(),
            file = piece.getPosition().substring(0, 1),
            rank = piece.getPosition().substring(1),
            ambiguousRank = false,
            ambiguousFile = false;

        /* We go through all of the pieces that could possibly be ambiguous. */
        state.each(function (posPiece) {
                       if (posPiece.getPosition() == piece.getPosition()) {
                           return;
                       }

                       var posFile = posPiece.getPosition().substring(0, 1);
                       var posRank = posPiece.getPosition().substring(1);

                       if (posPiece.getColor() == color && posPiece.getType() 
                           == type) {
                           if (pieceMoves(posPiece, {checkSquare : move[1]})) {
                               if (posFile == file) {
                                   ambiguousFile = true;
                               }

                               if (posRank == rank) {
                                   ambiguousRank = true;
                               }
                           }
                       }
                   });

        if (ambiguousFile) {
            inAlgebraicNotation += move[0].substring(0, 1);
        }

        if (ambiguousRank) {
            inAlgebraicNotation += move[0].substring(1);
        }

        // If a piece is being captured:
        if (state[move[1]]) {
            inAlgebraicNotation += "x";
        }

        inAlgebraicNotation += move[1];

        // TODO: Promotion

        return inAlgebraicNotation;
    }	

    /* Represents a chess piece on the board (or maybe it has been taken...). The
     * piece takes three arguments: its position, its type and its color. All of
     * the arguments have default values ("a1", "p" and "white", respectively),
     * but if the argument specified is both truthy (non-null, false...etc) and
     * invalid an error will be thrown. The valid arguments for position are any
     * position coordinates that are actually on the board, like "b2" or "h7";
     * case does not matter so "E4" would also be valid. The valid types are
     * defined by pieceTypes and pieceTypeNames, and are also case-insensitive.
     * The valid colors are "white", "w", "black" and "b", yet again
     * case-insensitive.
     */
    function Piece(position, type, color, id) {
        // Defaults:
        position = position || "a1";
        type = type || "p";
        color = color || "white";
        id = id || position + "";

        position = position.toLowerCase();
        color = color.toLowerCase();

        if (!isPositionValid(position)) {
            throw "Invalid position supplied to Piece! (" + position + " is not "
                + "valid.)";
        }

        if (indexOf(type, pieceTypes) < 0) {
            if (indexOf(type, pieceTypeNames) >= 0) {
                type = pieceTypes[indexOf(type, pieceTypeNames)];
            } else {
                throw "Invalid type supplied to Piece! (" + type 
                    + " is not valid.)";
            }
        }

        if (color != "w" && color != "b") {
            if (color == "white") {
                color = "w";
            } else if (color == "black") {
                color = "b";
            } else {
                throw "Invalid color supplied to Piece! (" + color + " is not "
                    + "valid.)";	
            }
        }

        /* Returns the piece's position as a string. The position is in the
         * standard letter + number format.
         */
        this.getPosition = function () {
            return position;
        };

        /* Sets this piece's position. */
        this.setPosition = function (newPosition) {
            position = newPosition;
        };

        /* Returns the piece's type. If full is true, then it returns the full
         * name of the type.
         */
        this.getType = function (full) {
            if (full) {
                return pieceTypeNames[indexOf(type, pieceTypes)];
            }
            return type;
        };

        /* Returns the piece's color as a single letter. */
        this.getColor = function () {
            return color;
        };

        /* Returns a new Piece instance that is the same as this one. */
        this.copy = function() {
            return new Piece(position, type, color, id);
        };

        /* Returns whether the specified piece is the same as this one. A piece
         * is the same as another piece if it has the same id, regardless of
         * whether it has moved or changed in the meantime.
         */
        this.equals = function (piece, otherId) {
            if (!otherId) {
                return piece.equals(this, id);
            } else {
                return id == otherId;
            }
        };

        /* Returns a string representation of the piece that has the type, color
         * and the position. For example, a black rook at c4 would be: bR@c4.
         */
        this.toString = function() {
            return color + type + "@" + position;
        };
    }

    /* A list of pieces. This contains all of the pieces on the board and has
     * methods for iterating through the pieces and retrieving pieces based on
     * various criteria. Accepts an array of pieces as an optional argument.
     * Returns a copy of the given array.
     */
    function PieceList(pieces) {    
        pieces = pieces || [];

        var that = [];

        for (var i = 0; i < pieces.length; i++) {
            that.push(pieces[i].copy());
        }

        /* Applies the given function to each of the pieces in the list. */
        that.each = function (func) {
            for (var i = 0; i < that.length; i++) {
                func(that[i]);		
            }
        };

        /* Returns the piece at the given position. If no piece is at that
         * position, then null is returned. 
         */
        that.get = function (position) {
            var piece = null;
            that.each(
                function (p) {
                    if (p.getPosition() == position) {
                        piece = p;
                    }
                });

            return piece;
        };

        /* Sets a particular piece to a particular position, adding the piece if
         * it was not in the list before. If there was a piece with the same
         * position in the list before, it is removed from the list and returned;
         * otherwise, null is returned.
         */
        that.set = function (position, piece) {
            var removedPiece = null;
            
            for (var i = 0; i < that.length; i++) {
                if (that[i].getPosition() == position) {
                    removedPiece = that[i];
                    that.splice(i, 0);
                }
            }

            piece.setPosition(position);
            return removedPiece;
        };

        /* Remove the piece at the given position. */
        that.removeAt = function (position) {
            for (var i = 0; i < that.length; i++) {
                if (that[i].getPosition() == position) {
                    that.splice(i, 1);
                }
            }
        };

        /* Returns whether the specified piece is in the list. */
        that.contains = function (piece) {
            var hasPiece = false;

            that.each(
                function (p) {
                    if (p.equals(piece)) {
                        hasPiece = true;
                    }
                });

            return hasPiece;
        };
        
        return that;
    }
}