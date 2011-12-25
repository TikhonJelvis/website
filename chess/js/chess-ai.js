//Chess artificial intelligence.  Users should primarily be concerned with functions that create boards and states and the makeAIPlayer function.


//GAME-AI

//Note: some functions here use pairs (arrays of 2 items).

DEBUG_MODE = false;

var ChessAI = new function() {
	function dbg(msg) {
	    //Writes a debug message.
	    if(DEBUG_MODE && console !== undefined) {
		console.log(msg);
	    }
	}

	function defaultHeuristic(state) {
	    //By default, every state is considered equal.
	    return 0;
	}

	var LOSE = -Infinity;
	var WIN = Infinity;

	function defaultGoalTest(state, succs) {
	    //By default, you lose if you have no more moves.
	    return succs.length === 0 ? null : LOSE;
	}
	function sortBy(xs, scorer) {
	    //Sorts a list where items' positions are determined by calling scorer on them.
	    var tagged = xs.map(function(x) {
		    return [scorer(x), x];
		});
	    tagged.sort();
	    return tagged.map(function(x) { return x[1]; });
	}

	function alphaBeta(initialState, kwargs) {
	    //Searches using minimax with alpha-beta pruning.  Returns a pair of a linked list of expected moves (where the first is the best move for the current player) and the current position's score (according to the current player; higher is better).  A linked list is represented as nested pairs; for example, the list (1 2 3) is represented as [1, [2, [3, null]]].
	    var depth = kwargs.depth;
	    if(typeof(depth) != 'number') {
		throw new Exception('depth is not a number');
	    }
	    var succFn = kwargs.successorFn;
	    var heur = kwargs.heuristic || defaultHeuristic;
	    var goalTest = kwargs.goalTest || defaultGoalTest;
	    var alpha = kwargs.alpha || LOSE;
	    var beta = kwargs.beta || WIN;
	    if(depth <= 0) {
		return [null, heur(initialState)];
	    }
	    var succs = succFn(initialState);
	    var goalResult = goalTest(initialState, succs);
	    if(goalResult !== null) {
		return [null, goalResult];
	    }
	    if(depth >= 3) {
		succs = sortBy(succs, function(succ) {
			return heur(succ[1]);
		    });
	    }
	    var bestMovePath = [succs[0][0], null];
	    for(var si = 0; si < succs.length; si++) {
		var succ = succs[si];
		var outcome = 
		    alphaBeta(succ[1],
			      {"successorFn" : succFn,
			       "depth" : depth - 1,
			       "heuristic" : heur,
			       "goalTest" : goalTest,
			       "alpha" : -beta,
			       "beta" : -alpha});
		var outcomeScore = -outcome[1];
		if(alpha < outcomeScore) {
		    alpha = outcomeScore;
		    bestMovePath = [succ[0], outcome[0]];
		}
		if(alpha >= beta) {
		    break;
		}	    
				
	    }
	    return [bestMovePath, alpha];
	}
	
	//CHESS-UTILS

	//Codes for pieces.
	var PAWN = this.PAWN = 0;
	var KNIGHT = this.KNIGHT = 1;
	var BISHOP = this.BISHOP = 2;
	var ROOK = this.ROOK = 3;
	var QUEEN = this.QUEEN = 4;
	var KING = this.KING = 5;

	//Codes for colors.
	var WHITE = this.WHITE = 0;
	var BLACK = this.BLACK = 1;



	var Piece = this.Piece = function(color, type) {
	    //Represents a piece.
	    //for example: white king is: new Piece(WHITE, KING)
	    this.color = color;
	    this.type = type;
	};

	Piece.prototype.hasAnyType = function(types) {
	    //Returns true iff this piece's type is one of the types given in the types array.
	    for(var i = 0; i < types.length; i++) {
		if(this.type === types[i]) {
		    return true;
		}
	    }
	    return false;
	};

	function pieceSafeColor(piece) {
	    //Get the color of a piece, or null if the piece is null.
	    return piece && piece.color;
	}

	function oppositeColor(color) {
	    //Get the opposite color.
	    return [1, 0][color];
	}

	function locToX(locString) {
	    //I don't know what this was intended to do, but it is not implemented.  Do not use.
	    throw new Exception('oh no');
	}

	function locToY(locString) {
	    //see above
	    throw new Exception('oh no');
	}
	function locToSquare(locString) {
	    //see above
	    return [locToX(locString), locToY(locString)];
	}

	//Pieces can be associated with characters.  The usual piece type abbreviations (pawn p, knight n, bishop b, rook r, queen q, king k) are used.  White pieces have uppercase characters; black pieces have lowercase characters. The absence of a piece is represented by a dash (-).


	//a map of piece code (integer) to uppercase piece character
	var pieceCharConversions = ['P', 'N', 'B', 'R', 'Q', 'K'];

	//the reverse
	var charPieceConversions = {};
	for(var p in pieceCharConversions) {
	    charPieceConversions[pieceCharConversions[p]] = p;
	}

	var pieceToChar = this.pieceToChar = function(piece) {
	    //Gets the character associated with a certain piece.
	    if(piece === null) {
		return '-';
	    }
	    var res = pieceCharConversions[piece.type];
	    if(res === undefined) { return undefined; }
	    return piece.color === 0 ? res : res.toLowerCase();
	};

	var charToPiece = this.charToPiece = function(ch) {
	    //Gets the piece associated with a certain character.
	    if(ch == '-') {
		return null;
	    }
	    var up = ch.toUpperCase();
	    var type = charPieceConversions[up];
	    if(type === undefined) { return undefined; }
	    return new Piece(ch == up ? 0 : 1, parseInt(type));
	};

	function writableToString() {
	    //This can be the toString method of as object that has a write method that, given a list of strings, appends strings to that list such that the result, when concatenated, is its string representation.  This is more efficient than ordinary string concatenation.
	    var strs = [];
	    this.write(strs);
	    return strs.join('');
	}

	var Board = this.Board = function(pieces) {
	    //A board is represented as a list of 64 pieces.  The argument (pieces) must be a list of 64 items, either pieces or null.  In the board, white is on the bottom and black is on the top.  When dealing with coordinates into the board, x and y must be from 0 to 7.  A low x corresponds to the queen side and a low y corresponds to black.
	    if(pieces.length !== 64) {
		throw new Exception('Need 64 pieces, got ' + pieces.length);
	    }
	    this.pieces = pieces;
	};
	var stringToBoard = this.stringToBoard = function(str) {
	    //Converts a string of 64 characters to a board, using piece/character conversions above.
	    var pieces = [];
	    for(var i = 0; i < str.length; i++) {
		var piece = charToPiece(str[i]);
		if(piece !== undefined) {
		    pieces.push(piece);
		}
	    }
	    return new Board(pieces);
	};

	var initBoard = this.initBoard = function() {
	    //The initial board.  Note that black is on top.
	    return stringToBoard
	    ('rnbqkbnr'+
	     'pppppppp'+
	     '--------'+
	     '--------'+
	     '--------'+
	     '--------'+
	     'PPPPPPPP'+
	     'RNBQKBNR');	   
	};
	Board.prototype.getPiece = function(x, y) {
	    //Get a piece (or null) from a board, given coordinates.
	    return this.pieces[x + y * 8];
	};
	Board.prototype.write = function(strs) {
	    //Writes the board as 8 lines of 8 characters each, representing the pieces.
	    for(var y = 0; y < 8; y++) {
		for(var x = 0; x < 8; x++) {
		    strs.push(pieceToChar(this.getPiece(x, y)));
		}
		strs.push('\n');
	    }
	};
	Board.prototype.toString = writableToString;

	Board.prototype.squareEmpty = function(x, y) {
	    //Tests if a given coordinate is empty.
	    return null === this.getPiece(x, y);
	};

	Board.prototype.pieceAtHasColor = function(x, y, color) {
	    //Tests if a piece at a certain location exists and has the given color.
	    var piece = this.getPiece(x, y);
	    return piece && (color === null || piece.color === color);
	};

	Board.prototype.pieceAtHasOppositeColor = function(x, y, color) {
	    //Tests if a piece at a certain location exists and has the opposite of a given color.
	    return this.pieceAtHasColor(x, y, oppositeColor(color));
	};

	Board.prototype.setPiece = function(x, y, piece) {
	    //Sets the piece at a certain location to a certain piece (which may be null).
	    this.pieces[x + y * 8] = piece;
	};

	Board.prototype.removePiece = function(x, y) {
	    //Removes a piece at a location, if there is one.
	    this.setPiece(x, y, null);
	};

	Board.prototype.movePiece = function(x1, y1, x2, y2) {
	    //Moves a piece at one location to another location.  Anything previously in the new location will get clobbered.
	    this.setPiece(x2, y2, this.getPiece(x1, y1));
	    this.removePiece(x1, y1);
	};


	Array.prototype.clone = function() {
	    //Clones an array shallowly.
	    var res = [];
	    for(var i = 0; i < this.length; i++) {
		res[i] = this[i];
	    }
	    return res;
	};

	Board.prototype.clone = function() {
	    //Clones a board shallowly.
	    return new Board(this.pieces.clone());
	};

	var Bstate = this.Bstate = function(kwargs) {
	    //A bstate represents all information about the state of the game, except move repetition info.  Kwargs (if provided) should be an object that may define the state's board, current turn (a color), whether each side can castle on the queen's side (a pair of booleans), whether each side can castle on the king's side (same), and the x coordinate of the pawn that made a double move on the previous turn, if any.
	    if(kwargs === undefined) { kwargs = {}; }
	    this.board = kwargs.board || initBoard();
	    this.turn = kwargs.turn || 0;
	    this.canCastleQ = kwargs.canCastleQ || [true, true];
	    this.canCastleK = kwargs.canCastleK || [true, true];
	    this.doubleMovePawn = kwargs.doubleMovePawn || null;
	};

	var stringToBstate = this.stringToBstate = function(str, kwargs) {
	    //Converts a string representing a board, along with extra arguments, to a Bstate.
	    if(kwargs === undefined) {
		kwargs = {};
	    }
	    kwargs.board = stringToBoard(str);
	    return new Bstate(kwargs);
	};

	Bstate.prototype.canKingCastle = function(side) {
	    //Whether or not the king of a certain color can castle.
	    return this.canCastleK[side];
	};

	Bstate.prototype.stopKingCastle = function(side) {
	    //Stops the king of a certain color from castling.
	    this.canCastleK[side] = false;
	};

	Bstate.prototype.canQueenCastle = function(side) {
	    //Whether or not the queen of a certain color can castle.
	    return this.canCastleQ[side];
	};

	Bstate.prototype.stopQueenCastle = function(side) {
	    //Stops the queen of a certain color from castling.
	    this.canCastleQ[side] = false;
	};

	Bstate.prototype.stopAllCastle = function(side) {
	    //Stops all castling of a certain color.
	    this.stopKingCastle(side);
	    this.stopQueenCastle(side);
	};
	Bstate.prototype.anyCanCastle = function(side) {
	    //Whether a certain color is allowed to castle at all.
	    return this.canKingCastle(side) ||
	    this.canQueenCastle(side);
	};

	Bstate.prototype.clone = function() {
	    //Clones a Bstate.
	    return new Bstate
	    ({'board' : this.board.clone(),
	      'turn' : this.turn,
	      'canCastleK' : this.canCastleK.clone(),
	      'canCastleQ' : this.canCastleQ.clone(),
	      'doubleMovePawn' : this.doubleMovePawn});
	};

	Bstate.prototype.write = function(strs) {
	    //Writes a bstate.  This string representation is unique to the Bstate (and equivalent ones) and can be used as a key in a dictionary.
	    strs.push('turn: ');
	    strs.push(String(this.turn));
	    strs.push('\n');
	    function addCastling(name, vec) {
		strs.push(name);
		strs.push(': ');
		strs.push(String(vec[0]));
		strs.push(', ');
		strs.push(String(vec[1]));
		strs.push('\n');
	    }
	    addCastling('canCastleK', this.canCastleK);
	    addCastling('canCastleQ', this.canCastleQ);
	    strs.push('doubleMovePawn: ');
	    strs.push(String(this.doubleMovePawn));
	    strs.push('\n');
	    this.board.write(strs);
	};
	Bstate.prototype.toStringUncached = writableToString;

	Bstate.prototype.toString = function() {
	    if(this.stringRep === undefined) {
		this.stringRep = this.toStringUncached();
	    }
	    return this.stringRep;
	};

	var State = this.State = function(kwargs) {
	    //A state represents the state of the game.  It contains a Bstate and information about move repitition.  Kwargs can specify the base (Bstate), a dictionary connecting the string representation of a Bstate to how many times it has occurred since the last permanent change (pawn move/castle invalidating move/capture), and the number of plies (half-turns) since the last permanent change, not including the current ply.
	    if(kwargs === undefined) { kwargs = {}; }
	    this.base = kwargs.base || new Bstate();
	    this.bstatesSinceProgress =
	    kwargs.bstatesSinceProgress || {};
	    this.pliesSinceProgress = 0;
	};

	var stringToState = this.stringToState = function(str, bkwargs, kwargs) {
	    //Converts a string representation of a board, arguments given to the Bstate constructor, and arguments given to the State constructor to a State.
	    if(kwargs === undefined) {
		kwargs = {};
	    }
	    kwargs.base = stringToBstate(str, bkwargs);
	    return new State(kwargs);
	};

	State.prototype.hasThreeMoveRepetition = function() {
	    //Whether or not a three-move repetition has happened in this State.
	    var reps = this.bstatesSinceProgress[this.base.toString()];
	    return reps && reps >= 2;
	};

	function copyHashtable(ht) {
	    //Makes a shallow copy of a hashtable.
	    var res = {};
	    for(var k in ht) {
		res[k] = ht[k];
	    }
	    return res;
	}
	State.prototype.addMoveRepetition = function(bstate) {
	    //Returns a new move repetition dictionary, taking into account the next Bstate.
	    var res = copyHashtable(this.bstatesSinceProgress); 
	    var count = this.bstatesSinceProgress[bstate];
	    res[bstate] = (count && count + 1) || 1;
	    return res;
	};

	State.prototype.hasNoProgressStalemate = function() {
	    //Whether a tie has occurred due to lack of progress.
	    return this.pliesSinceProgress >= 100;
	};

	State.prototype.getBoard = function() {
	    //Gets the state's board.
	    return this.base.board;
	};
	State.prototype.toString = function() {
	    return this.base.toString();
	};

	//These directions are represented as an (x, y) pair representing a single step in that direction.  North is towards black; east is king side.
	var NORTH = [0, -1];
	var SOUTH = [0, 1];
	var EAST = [1, 0];
	var WEST = [-1, 0];
	var NORTHEAST = [1, -1];
	var NORTHWEST = [-1, -1];
	var SOUTHEAST = [1, 1];
	var SOUTHWEST = [-1, 1];


	//These direction lists represent the directions that a rook, bishop, and queen can move respectively.
	var ORTHOGONAL_DIRECTIONS = [NORTH, SOUTH, EAST, WEST];
	var DIAGONAL_DIRECTIONS = [NORTHEAST, NORTHWEST, SOUTHEAST, SOUTHWEST];
	var DIRECTIONS = ORTHOGONAL_DIRECTIONS.concat(DIAGONAL_DIRECTIONS);

	function validX(x) {
	    //Whether an x coordinate is valid.
	    return 0 <= x && x < 8;
	}
	function validY(y) {
	    //Whether a y coordinate is valid.
	    return 0 <= y && y < 8;
	}
	function validSquare(x, y) {
	    //Whether a set of coordinates is valid.
	    return validX(x) && validY(y);
	}

	//Moves are represented as lists.  They may be one of: 
	//['move', moveIsCapture, x1, y1, x2, y2]
	//['en-passant', x1, y1, x2, y2]
	//['promote', type, x1, y1, x2, y2]
	//['castle', 'king']
	//['castle', 'queen']

	Bstate.prototype.straightMovesInDir = function(color, x, y, dir) {
	    //Returns a list of moves that a piece of a certain color at (x, y) can move to in direction dir.
	    var dx = dir[0], dy = dir[1];
	    //list of resulting moves
	    var moves = [];
	    for(var newx = x + dx, newy = y + dy; validSquare(newx, newy); newx += dx, newy += dy) {
		//piece at target square
		var piece = this.board.getPiece(newx, newy);
		if(piece && piece.color === color) {
		    //trying to move on top of a piece fo the same color fails
		    return moves;
		}
		//allow the move
		moves.push(['move', piece !== null, x, y, newx, newy]);
		if(piece) {
		    //if it's trying to move onto another piece of a different color, then it can still move onto it, but can move no further
		    return moves;
		}
	    }
	    return moves;
	};
	Bstate.prototype.straightMovesInDirs = function(color, x, y, dirs) {
	    //As before, but the piece can move in multiple directions.
	    var moves = [];
	    for(var di = 0; di < dirs.length; di++) {
		var dirMoves = this.straightMovesInDir(color, x, y, dirs[di]);
		for(var i = 0; i < dirMoves.length; i++) {
		    moves.push(dirMoves[i]);
		}
	    }
	    return moves;
	};

	Bstate.prototype.rookMoves = function(color, x, y) {
	    //What moves a rook can make.
	    return this.straightMovesInDirs(color, x, y, ORTHOGONAL_DIRECTIONS);
	};

	Bstate.prototype.bishopMoves = function(color, x, y) {
	    //What moves a bishop can make.
	    return this.straightMovesInDirs(color, x, y, DIAGONAL_DIRECTIONS);
	};

	Bstate.prototype.queenMoves = function(color, x, y) {
	    //What moves a queen can make.
	    return this.straightMovesInDirs(color, x, y, DIRECTIONS);
	};

	Bstate.prototype.squareMoveLegal = function(color, x, y) {
	    //Whether it is possibly legal for a piece of a certain color to move to (x, y)
	    return validSquare(x, y) && !this.board.pieceAtHasColor(x, y, color);
	};

	//Knights and kings move relatively.  These lists are lists of relative coordinates that the pieces can move to.

	var KNIGHT_RELATIVE_MOVES = 
	    [[1, 2], [2, 1],
	     [-1, 2], [-2, 1],
	     [1, -2], [2, -1],
	     [-1, -2], [-2, -1]];

	var KING_RELATIVE_MOVES = DIRECTIONS;

	Bstate.prototype.relativeMoves = function(color, x, y, steps) {
	    //Gets a list of relative moves a piece can make.
	    var result = [];
	    for(var si = 0; si < steps.length; si++) {
		var step = steps[si];
		var newx = x + step[0], newy = y + step[1];
		if(this.squareMoveLegal(color, newx, newy)) {
		    result.push
			(['move', null !== this.board.getPiece(newx, newy),
			  x, y, newx, newy]);
		}
	    }
	    return result;
	};

	Bstate.prototype.knightMoves = function(color, x, y) {
	    //What moves a knight can make.
	    return this.relativeMoves(color, x, y, KNIGHT_RELATIVE_MOVES);
	};

	Bstate.prototype.kingMoves = function(color, x, y) {
	    //What moves a king can make.
	    return this.relativeMoves(color, x, y, KING_RELATIVE_MOVES);
	};

	function colorForwardDy(color) {
	    //The relative y coordinate representing a 1-step forward movement for a color.
	    if(color === 0) {
		return -1;
	    } else {
		return 1;
	    }
	    //return [-1, 1][color];
	}

	function colorEnPassantY(color) {
	    //The y-coordinate that is right after a color's initial pawn y coordinate.
	    return [2, 5][color];
	}
	function colorPromotingY(color) {
	    //The y-coordiante at which pawns of the color promote.
	    return [0, 7][color];
	}
	function colorPawnStartingY(color) {
	    //The y-coordinate at which pawns of the color start.
	    return [6, 1][color];
	}
	function colorBaseY(color) {
	    //The y-coordinate at which non-pawns of the color start.
	    return [7, 0][color];
	}
	Bstate.prototype.pawnCapturableSquare = function(color, x, y) {
	    //Whether or not a square can be captured by a pawn whose turn it is and who is behind and diagonal to the square.  If the capture is a normal capture, 'move' is returned; if it is an en passent capture, 'en-passant' is returned.
	    if(!validX(x)) { return false; }
	    if(this.board.pieceAtHasOppositeColor(x, y, color)) {
		return 'move';
	    }
	    if(y === colorEnPassantY(color) &&
	       x === this.doubleMovePawn) {
		return 'en-passant';
	    }
	};

	//What pieces can be promoted to.
	var PROMOTABLE_PIECES = [KNIGHT, BISHOP, ROOK, QUEEN];

	Bstate.prototype.pawnMoves = function(color, x, y) {
	    //What moves a pawn can make.
	    var forwardDy = colorForwardDy(color);
	    var forward1y = y + forwardDy;
	    var forward2y = forward1y + forwardDy;
	    var promoting = forward1y === colorPromotingY(color);
	    var dests = [];
	    var moves = [];
	    if(this.board.squareEmpty(x, forward1y)) {
		dests.push([false, x, y, x, forward1y]);
		var starting = y === colorPawnStartingY(color);
		if(starting && this.board.squareEmpty(x, forward2y)) {
		    dests.push([false, x, y, x, forward2y]);
		}
	    } 
	    for(var newx = x - 1; newx <= x + 1; newx += 2) {
		var move = this.pawnCapturableSquare(color, newx, forward1y);
		if(move == 'en-passant') {
		    moves.push(['en-passant', x, y, newx, forward1y]);
		} else if(move == 'move') {
		    dests.push([true, x, y, newx, forward1y]);
		}
	    }
	    for(var di = 0; di < dests.length; di++) {
		if(promoting) {
		    for(var pi = 0; pi < PROMOTABLE_PIECES.length; pi++) {
			moves.push(['promote', PROMOTABLE_PIECES[pi]]
				   .concat(dests[di].slice(1)));
		    }
		} else {
		    moves.push(['move'].concat(dests[di]))
		}
	    }
	    return moves;
	};

	Bstate.prototype.pieceMoves = function(x, y) {
	    //What moves a piece at a certain location can make.
	    var piece = this.board.getPiece(x, y);
	    if(piece === null) { return []; }
	    var color = piece.color;
	    return piece.type === PAWN ? this.pawnMoves(color, x, y) :
	    piece.type === KNIGHT ? this.knightMoves(color, x, y) :
	    piece.type === BISHOP ? this.bishopMoves(color, x, y) :
	    piece.type === ROOK ? this.rookMoves(color, x, y) :
	    piece.type === QUEEN ? this.queenMoves(color, x, y) :
	    piece.type === KING ? this.kingMoves(color, x, y) : [];
	};
	Bstate.prototype.doMove = function(move) {
	    //Executes a move, modifying the state in the process.  Returns true iff a permanent change has happened.
	    var board = this.board;
	    var turn = this.turn;
	    var permChange = false;
	    var moveType = move[0];
	    if(moveType == 'move') {
		this.doubleMovePawn = null;
		var x1 = move[2];
		var y1 = move[3];
		var x2 = move[4];
		var y2 = move[5];
		var pieceType = board.getPiece(x1, y1).type;
		if(pieceType === ROOK) {
		    if(x1 === 0) {
			permChange = this.canQueenCastle(turn);
			this.stopQueenCastle(turn);
		    } else if(x1 === 7) {
			permChange = this.canKingCastle(turn);
			this.stopKingCastle(turn);
		    }
		} else if(pieceType == KING) {
		    permChange = this.anyCanCastle(turn);
		    this.stopAllCastle(turn);
		} else if(pieceType == PAWN) {
		    permChange = true;
		    var dy = y1 - y2;
		    if(dy === 2 || dy === -2) {
			this.doubleMovePawn = x1;
		    }
		}
		if(move[1]) {
		    permChange = true;
		}
		board.movePiece(x1, y1, x2, y2);
	    } else if(moveType == 'castle') {
		permChange = true;
		this.stopAllCastle(turn);
		var baseY = colorBaseY(turn);
		var castleType = move[1];
		if(castleType == 'king') {
		    board.movePiece(4, baseY, 6, baseY);
		    board.movePiece(7, baseY, 5, baseY);
		} else if(castleType == 'queen') {
		    board.movePiece(4, baseY, 2, baseY);
		    board.movePiece(0, baseY, 3, baseY);
		}
	    } else if(moveType == 'promote') {
		permChange = true;
		var type = move[1];
		var x1 = move[2];
		var y1 = move[3];
		var x2 = move[4];
		var y2 = move[5];
		board.removePiece(x1, y1);
		board.setPiece(x2, y2, new Piece(turn, type));
	    } else if(moveType == 'en-passant') {
		permChange = true;
		var x1 = move[1];
		var y1 = move[2];
		var x2 = move[3];
		var y2 = move[4];
		board.movePiece(x1, y1, x2, y2);
		board.removePiece(x2, y1);
	    }
	    this.turn = oppositeColor(this.turn);
	    return permChange;
	};

	Board.prototype.validAttacker = function(x, y, color, types) {
	    //Whether a piece at a certain location of a certain color has a certain type.  If color is null, the piece may be either.
	    if(!validSquare(x, y)) { return false; }
	    var piece = this.getPiece(x, y);
	    if(piece === null) { return false; }
	    if(color !== null && color !== piece.color) { return false; }
	    return piece.hasAnyType(types);
	};

	Bstate.prototype.pawnsAttacking = function(x, y, color) {
	    //Whether any pawns attack a square.  Ignores en-passant (irrelevant if the attacked piece is not a pawn).
	    var colors = color === null ? [0, 1] : [color];
	    for(var ci = 0; ci < colors.length; ci++) {
		var pawnColor = colors[ci];
		var pawnY = y - colorForwardDy(pawnColor);
		for(var pawnX = x - 1; pawnX <= x + 1; pawnX += 2) {
		    if(this.board.validAttacker(pawnX, pawnY, pawnColor, [PAWN])) {
			return true;
		    }
		}
	    }
	    return false;
	};

	Bstate.prototype.relativeMoversAttacking = function(x, y, types, color, moves) {
	    //Whether any relative movers of the types attack a square.
	    for(var mi = 0; mi < moves.length; mi++) {
		var move = moves[mi];
		var ax = x + move[0];
		var ay = y + move[1];
		if(this.board.validAttacker(ax, ay, color, types)) {
		    return true;
		}
	    }
	    return false;
	};

	Bstate.prototype.knightsAttacking = function(x, y, color) {
	    //Whether any knights attack a square.
	    return this.relativeMoversAttacking(x, y, [KNIGHT], color, KNIGHT_RELATIVE_MOVES);
	};

	Bstate.prototype.kingsAttacking = function(x, y, color) {
	    //Whether any bishops attack a square.
	    return this.relativeMoversAttacking(x, y, [KING], color, KING_RELATIVE_MOVES);
	};

	Bstate.prototype.straightAttackerInDir = function(x, y, types, color, dir) {
	    //Whether any straight movers attack a square.
	    var dx = dir[0], dy = dir[1];
	    for(var ax = x + dx, ay = y + dy; validSquare(ax, ay); ax += dx, ay += dy) {
		var piece = this.board.getPiece(ax, ay);
		if(piece !== null) {
		    if(color !== null && color !== piece.color) {
			return false;
		    }
		    for(var i = 0; i < types.length; i++) {
			if(types[i] === piece.type) {
			    return true;
			}
		    }
		    return false;
		}
	    }
	    return false;
	};

	Bstate.prototype.straightAttackersInDirs = function(x, y, types, color, dirs)  {
	    //Whether any straight attackers of the types attack a square.
	    for(var i = 0; i < dirs.length; i++) {
		if(this.straightAttackerInDir(x, y, types, color, dirs[i])) {
		    return true;
		}
	    }
	    return false;
	};

	Bstate.prototype.piecesAttacking = function(x, y, color) {
	    //Whether any pieces attack a square.
	    if(color === undefined) {
		color = oppositeColor(pieceSafeColor(this.board.getPiece(x, y)));
	    }
	    if(this.pawnsAttacking(x, y, color)) { 
		//dbg('pawn attacking');
		return true; 
	    }
	    if(this.knightsAttacking(x, y, color)) { 
		//dbg('knight attacking');
		return true;
	    }
	    if(this.kingsAttacking(x, y, color)) { 
		//dbg('king attacking');
		return true; 
	    }
	    if(this.straightAttackersInDirs(x, y, [ROOK, QUEEN], color, ORTHOGONAL_DIRECTIONS)) { 
		//dbg('rook/queen atacking');
		return true; 
	    }
	    if(this.straightAttackersInDirs(x, y, [BISHOP, QUEEN], color, DIAGONAL_DIRECTIONS)) { 
		//dbg('bishop/queen attacking');
		return true;
	    }
	    //dbg('nothing attacking');
	    return false;
	};

	Bstate.prototype.squaresAttackedByPawn = function(x, y) {
	    //A list of squares attacked by a pawn.
	    var color = this.board.getPiece(x, y).color;
	    var forwardY = y + colorForwardDy(color);
	    var result = [];
	    for(var tx = x - 1; tx <= x + 1; tx += 2) {
		if(validX(tx)) {
		    result.push([tx, forwardY]);
		}
	    }
	    return result;
	};

	Bstate.prototype.squaresAttackedRelative = function(x, y, offsets) {
	    //A list of squares attacked by a relative mover.
	    var result = [];
	    for(var i = 0; i < offsets.length; i++) {
		var offset = offsets[i];
		var newx = x + offset[0], newy = y + offset[1];
		if(validSquare(newx, newy)) {
		    result.push([newx, newy]);
		}
	    }
	    return result;
	};

	Bstate.prototype.squaresAttackedInDirection = function(x, y, dir, acc) {
	    //A list of squares attacked by a piece moving in a certain direction.
	    if(acc === undefined) { acc = []; }
	    var dx = dir[0], dy = dir[1];
	    for(var tx = x + dx, ty = y + dy; validSquare(tx, ty); tx += dx, ty += dy) {
		acc.push([tx, ty]);
		if(this.board.getPiece(tx, ty)) {
		    return acc;
		}
	    }
	    return acc;
	};

	Bstate.prototype.squaresAttackedInDirections = function(x, y, dirs) {
	    //A list of squares attacked by a piece moving in certain directions.
	    var acc = [];
	    for(var i = 0; i < dirs.length; i++) {
		this.squaresAttackedInDirection(x, y, dirs[i], acc);
	    }
	    return acc;
	};

	Bstate.prototype.squaresAttackedBy = function(x, y) {
	    //A list of squares attacked by a piece at a given location.
	    var board = this.board;
	    var piece = board.getPiece(x, y);
	    var type = piece.type;
	    if(type == PAWN) {
		return this.squaresAttackedByPawn(x, y);
	    } else if(type == KNIGHT) {
		return this.squaresAttackedRelative(x, y, KNIGHT_RELATIVE_MOVES);
	    } else if(type == KING) {
		return this.squaresAttackedRelative(x, y, KING_RELATIVE_MOVES);
	    } else if(type == BISHOP) {
		return this.squaresAttackedInDirections(board, x, y, DIAGONAL_DIRECTIONS);
	    } else if(type == ROOK) {
		return this.squaresAttackedInDirections(board, x, y, ORTHOGONAL_DIRECTIONS);
	    } else if(type == QUEEN) {
		return this.squaresAttackedInDirections(board, x, y, DIRECTIONS);
	    }
	};

	Bstate.prototype.piecesAttackedBy = function(x, y, color) {
	    //A list of pieces of a certain color attacked by a piece at a certain location.
	    if(color === undefined) {
		color = oppositeColor(this.board.getPiece(x, y).color);
	    }
	    var squares = this.squaresAttackedBy(x, y);
	    var res = [];
	    for(var i = 0; i < squares.length; i++) {
		var square = squares[i];
		var piece = this.board.getPiece(square[0], square[1]);
		if(piece !== null && (color === null || color === piece.color)) {
		    res.push(square);
		}
	    }
	    return res;
	};

	Board.prototype.kingSquare = function(color) {
	    //The coordinates of the king of a certain color.
	    for(var y = 0; y < 8; y++) {
		for(var x = 0; x < 8; x++) {
		    var piece = this.getPiece(x, y);
		    if(piece !== null && piece.type === KING && 
		       piece.color === color) {
			return [x, y];
		    }
		}
	    }
	    throw new Error("No king of color " + color + " on board \n" + this.toString());
	};

	Bstate.prototype.piecesAttackingKing = function(color) {
	    //A list of locations of pieces attacking the king of a certain color.
	    if(color === undefined) {
		color = oppositeColor(this.turn);
	    }
	    var ks = this.board.kingSquare(color);
	    return this.piecesAttacking(ks[0], ks[1], oppositeColor(color));
	};

	Bstate.prototype.castlingMoves = function() {
	    //A list of castling moves that can be made by the player whose turn it is.
	    var turn = this.turn;
	    var opp = oppositeColor(turn);
	    var board = this.board;
	    var baseY = colorBaseY(turn);
	    var result = [];
	    function empty(x) {
		return board.squareEmpty(x, baseY);
	    }
	    var bstate = this;
	    function noAttack(x) {
		//return bstate.piecesAttacking(x, baseY, opp).length == 0;
		return !bstate.piecesAttacking(x, baseY, opp);
	    }
	    if(this.canKingCastle(turn) &&
	       empty(5) && empty(6) &&
	       noAttack(4) && noAttack(5)) {
		result.push(['castle', 'king']);
	    }
	    if(this.canQueenCastle(turn) &&
	       empty(3) && empty(2) && empty(1) &&
	       noAttack(4) && noAttack(3)) {
		result.push(['castle', 'queen']);
	    }
	    return result;
	};

	Bstate.prototype.allMoves = function() {
	    //A list of all legal moves (ignoring check) that the current player can make.
	    var res = [];
	    for(var y = 0; y < 8; y++) {
		for(var x = 0; x < 8; x++) {
		    var piece = this.board.getPiece(x, y);
		    if(piece !== null && piece.color === this.turn) {
			var moves = this.pieceMoves(x, y);
			for(var i = 0; i < moves.length; i++) {
			    res.push(moves[i]);
			}
		    }
		}
	    }
	    var castleMoves = this.castlingMoves();
	    for(var i = 0; i < castleMoves.length; i++) {
		res.push(castleMoves[i]);
	    }
	    return res;
	};

	State.prototype.nextState = function(nextBstate, permChange) {
	    //The next state that happens after this state, if its Bstate is the given one and permChange is true iff a permanent change has occurred.
	    return new State
	    ({'base': nextBstate,
	      'movesSinceProgress': 
	      permChange ? 0 : 1 + this.movesSinceProgress,
	      'bstatesSinceProgress':
	      permChange ? {} : this.addMoveRepetition(this.base)});
	};

	State.prototype.stateAfterMove = function(move) {
	    //The next state that happens after this state when a certain move takes place.  Returns null if the move results in check.
	    try {
		var newBstate = this.base.clone();
		var permChange = newBstate.doMove(move);
		//if(newBstate.piecesAttackingKing().length > 0) { 
		if(newBstate.piecesAttackingKing()) {
		    return null;
		}
		return this.nextState(newBstate, permChange);
	    } catch(ex) {
		ex.message = "Error when applying move " + move + " to state\n" + this + ":" + ex.message;
		throw ex;
	    }
	};

	function successorFunction(state) {
	    //A list of pairs of [move, newState], representing legal moves that can happen at a state and their resulting states.
	    var bstate = state.base;
	    var result = [];
	    var moves = bstate.allMoves();
	    for(var i = 0; i < moves.length; i++) {
		var move = moves[i];
		var newState = state.stateAfterMove(move);
		if(newState !== null) {
		    result.push([move, newState]);
		}
	    }
	    return result;
	}

	function goalTest(state, successors) {
	    //Given a state and a list of successors (optional), if the game has ended, returns a score; otherwise, null.
	    if(successors === undefined) {
		successors = successorFunction(state);
	    }
	    if(successors.length == 0) {
		var base = state.base;
		if(base.piecesAttackingKing(base.turn)) {
		    return LOSE; //checkmate
		} else {
		    return 0; //stalemate
		}
	    } else if(state.hasThreeMoveRepetition()) {
		return 0; //3-move repetition
	    } else if(state.hasNoProgressStalemate()) {
		return 0; //no progress
	    }
	    return null;
	}
	function movesEqual(m1, m2) {
	    //Whether two moves are equal.
	    if(m1.length !== m2.length) { return false; }
	    for(var i = 0; i < m1.length; i++) {
		if(m1[i] != m2[i]) {
		    return false;
		}
	    }
	    return true;
	}
	function applyMove(state, move) {
	    //The state that results after a move takes place.  This is null if the move is illegal.
	    var validMoves = state.base.allMoves();
	    for(var i = 0; i < validMoves.length; i++) {
		var vm = validMoves[i];
		if(movesEqual(move, vm)) {
		    return state.stateAfterMove(move);
		}
	    }
	    return null;
	}

	State.prototype.applyMove = function(move) {
	    //Applies a move to this state, returning the new state if the move is legal, otherwise null.
	    return applyMove(this, move);
	};

	State.prototype.convertStringMove = function(str) {
	    if(str == '0-0') {
		return ['castle', 'king'];
	    } else if(str == '0-0-0') {
		return ['castle', 'queen'];
	    } else {
		function convX(x) {
		    return x.toLowerCase().charCodeAt(0) - 'a'.charCodeAt(0);
		}
		function convY(y) {
		    return '8'.charCodeAt(0) - y.charCodeAt(0);
		}
		var x1 = convX(str[0]);
		var y1 = convY(str[1]);
		//str[2] = hyphen/space/whatever 
		var x2 = convX(str[3]);
		var y2 = convY(str[4]);
		var board = this.getBoard();
		var piece = board.getPiece(x1, y1);
		var isCapture = !board.squareEmpty(x2, y2);
		//Technically, isCapture will be false if the move is an en-passant.  This fact is used later.
		if(piece.type === PAWN) {
		    if(y2 === 0 || y2 === 7) { //promotion
			if(str[5] != '*') {
			    throw new Error("Promoting move does not have asterisk as character #5.");
			}
			var pieceType = charPieceConversions[str[6].toUpperCase()];
				
			return ['promote', pieceType, x1, y1, x2, y2];
		    } else if(!isCapture && x1 !== x2) { //en-passant
			return ['en-passant', x1, y1, x2, y2];
		    }
		}
		return ['move', isCapture, x1, y1, x2, y2];
	    }
	};

	State.prototype.applyStringMove = function(str) {
	    return this.applyMove(this.convertStringMove(str));
	};

	function moveToString(move) {
	    function convX(x) {
		return String.fromCharCode('a'.charCodeAt(0) + x);
	    }
	    function convY(y) {
		return String.fromCharCode('8'.charCodeAt(0) - y);
	    }
	    var moveType = move[0];
	    if(moveType == 'castle') {
		if(move[1] == 'king') {
		    return '0-0';
		} else {
		    return '0-0-0';
		}
	    }
	    var offset = moveType == 'move' || moveType == 'promote' ? 2 : 1;
	    var x1 = convX(move[offset]);
	    var y1 = convY(move[offset+1]);
	    var x2 = convX(move[offset+2]);
	    var y2 = convY(move[offset+3]);
	    var res = x1 + y1 + '-' + x2 + y2;
	    if(moveType == 'promote') {
		res += '*' + pieceCharConversions[move[1]];
	    }
	    return res;
	}
		
	    
    
	//HEURISTIC
						  
	function pointValue(piece) {
	    //The heuristic point value of a piece.
	    if(piece === null) { return 0; }
	    return piece.type === KING ? 100 :
		piece.type === QUEEN ? 9 :
		piece.type === ROOK ? 5 :
		piece.type === BISHOP ? 3 :
		piece.type === KNIGHT ? 3 :
		piece.type === PAWN ? 1 : 0;
	}
	//Different emphasis is placed on different aspects of the state.

	//Multiplied by the total point value of the pieces of a color.
	var EMPHASIS_POINT_VALUE = 3;

	//Multiplied by the total number of attacks a color can make, not including attacks by the king.
	var EMPHASIS_BOARD_CONTROL = .1;

	//Multiplied by the total number of attacks a color can make on the center, counting only pawns and knights.
	var EMPHASIS_CENTER = .7;

	//Multiplied by the total number of attacks made on a square next to the king.
	var EMPHASIS_KING_CONTROL = .5;

	//Multiplied by the total estimated point value gain in a fight (where many captures happen repeatedly at a given square).  This is lower than EMPHASIS_POINT_VALUE because fights are not guaranteed to turn out as predicted.
	var EMPHASIS_FIGHTING = 2.5;

	//Multiplied by the total number of squares that all points of a color have advanced.
	var EMPHASIS_PAWN_ADVANCEMENT = .4;

	function heuristic(state) {
	    //Get a number representing approximately how good a state is, based on some factors.
	    var bstate = state.base;
	    var board = bstate.board;
	    var colorCurr = bstate.turn;
	    var colorOther = oppositeColor(colorCurr);
	    //var locKingCurr, locKingOther;
	    var attackMap = [];
	    for(var i = 0; i < 8*8*2; i++) {
		attackMap.push([]);
	    }
	    function getAttackers(x, y, color) {
		return attackMap[color + 2 * (x + y * 8)];
	    }
	    /*function getKingLoc(color) {
	      if(color === colorCurr) { return locKingCurr; }
	      if(color === colorOther) { return locKingOther; }
	      return null;
	      }*/
	    function incPair(pair, inc) {
		if(inc !== null) {
		    pair[0] += inc[0];
		    pair[1] += inc[1];
		}
	    }
	    /*function valuePair(fun) {
	      return [fun(colorCurr), fun(colorOther)];
	      }
	      function colorPair(color, val) {
	      if(color === colorCurr) { return [val, 0]; }
	      if(color === colorOther) { return [0, val]; }
	      return null;
	      }*/
	    function ignoreLocationsTypes(locs, types) {
		var count = 0;
		for(var i = 0; i < locs.length; i++) {
		    var loc = locs[i];
		    var piece = board.getPiece(loc[0], loc[1]);
		    if(piece !== null && !piece.hasAnyType(types)) {
			count++;
		    }
		}
		return count;
	    }
	    /*function valueKingControl(x, y) {
	      return valuePair(function(color) {
	      var tot = 0;
	      var otherKing = getKingLoc(oppositeColor(color));
	      var loc = [x, y];
	      for(var i = 0; i < 2; i++) {
	      if(Math.abs(loc[i] - otherKing[i]) > 1) {
	      return 0;
	      }
	      }
	      var value = EMPHASIS_KING_CONTROL * getAttackers(x, y, color).length;
	      for(var i = 0; i < 2; i++) {
	      if((otherKing[i] <= 3 && 
	      loc[i] === otherKing[i] - 1) ||
	      (otherKing[i] >= 4 &&
	      loc[i] === otherKing[i] + 1)) {
	      tot += value;
	      }
	      }
	      return tot;
	      });
	      }*/
	    function scoreFight(pointValueTarget, selfPtList, selfUsed, otherPtList, otherUsed) {
		if(selfUsed == selfPtList.length) { 
		    //no more trading
		    return 0; 
		}
		if(otherUsed == otherPtList.length) {
		    //can take piece
		    return pointValueTarget;
		}
		var valueTrade = 
		    pointValueTarget - 
		    scoreFight(selfPtList[selfUsed], 
			       otherPtList, otherUsed, 
			       selfPtList, selfUsed + 1);
		return Math.max(0, valueTrade);
	    }
	    function fightingPoints(x, y, color) {
		var res = [];
		var attackers = getAttackers(x, y, color);
		for(var i = 0; i < attackers.length; i++) {
		    var coords = attackers[i];
		    res.push(pointValue(board.getPiece(coords[0], coords[1])));
		}
		res.sort();
		return res;
	    }
	    

    
	    var vPt = [0, 0];
	    var vBoardControl = [0, 0];
	    var vCenterControl = [0, 0];
	    var vKingControl = [0, 0];
	    var vFighting = [0, 0];
	    var vPawnAdvancement = [0, 0];
	    
	    for(var y = 0; y < 8; y++) {
		for(var x = 0; x < 8; x++) {
		    var piece = board.getPiece(x, y);
		    if(piece !== null) {
			/*if(piece.type === KING) {
			  if(piece.color == colorCurr) {
			  locKingCurr = [x, y];
			  } else {
			  locKingOther = [x, y];
			  }
			  }*/
			var targets = bstate.squaresAttackedBy(x, y);
			var loc = [x, y];
			for(var i = 0; i < targets.length; i++) {
			    var target = targets[i];
			    getAttackers(target[0], target[1], piece.color).push(loc);
			}
		    }
		}
	    }
	    for(var y = 0; y < 8; y++) {
		for(var x = 0; x < 8; x++) {
		    var piece = board.getPiece(x, y);
		    if(piece !== null) {
			var color = piece.color;
			vPt[color] += EMPHASIS_POINT_VALUE * pointValue(piece);
			if(piece.type === PAWN) {
			    vPt[color] += 
				EMPHASIS_PAWN_ADVANCEMENT * 
				colorForwardDy(color) *
				(y - colorPawnStartingY(color));
			}
			for(var c = 0; c < 2; c++) {
			    vBoardControl[c] += 
				EMPHASIS_BOARD_CONTROL *
				ignoreLocationsTypes(getAttackers(x, y, c), [KING]);
			}
			if(color === colorOther) {
			    var goodPoints = fightingPoints(x, y, colorCurr);
			    var evilPoints = fightingPoints(x, y, colorOther);
			    var targetPoints = pointValue(piece);
			    vFighting[colorCurr] += EMPHASIS_FIGHTING *
				scoreFight(targetPoints, goodPoints, 0, evilPoints, 0);
			}
		    }
		}
	    }
	    for(var y = 3; y <= 4; y++) {
		for(var x = 3; x <= 4; x++) {
		    for(var color = 0; color < 2; color++) {
			vCenterControl[color] +=
			    EMPHASIS_CENTER *
			    ignoreLocationsTypes(getAttackers(x, y, color), 
						 [KING, BISHOP, ROOK, QUEEN]);
		    }
		}
	    }
	    var vTotal = [0, 0];
	    incPair(vTotal, vPt);
	    incPair(vTotal, vBoardControl);
	    incPair(vTotal, vCenterControl);
	    incPair(vTotal, vKingControl);
	    incPair(vTotal, vFighting);
	    incPair(vTotal, vPawnAdvancement);
	    return vTotal[0] - vTotal[1] + (Math.random() - 0.5) / 100;
	}

	function searchChess(state, depth) {
	    //Calls alphaBeta with chess-specific arguments.  The result has the same format as that of alphaBeta.
	    return alphaBeta
		(state, {'successorFn' : successorFunction,
			'heuristic' : heuristic,
			'goalTest' : goalTest,
			'depth' : depth});
		 
	}

	var makeAIPlayer = this.makeAIPlayer = function(initDepth, state) {
	    //Returns a function that, given a move (in string format), will choose a good move for the current player to make in response (also in string format).  If the move is null, the AI gets to make the first move.  initDepth specified how deep to search.  More depth leads to better moves but much higher computation time.  A number from 3-5 is probably good.
	    if(state === undefined) {
		state = new State();
	    }
	    return function(strmove) {
		try {
		    dbg('[AI] getting move');
		    //dbg(state.base.allMoves());
		    var state2;
		    if(strmove === null) {
			state2 = state;
		    } else {
			state2 = state.applyStringMove(strmove);
			if(state2 === null) {
			    throw new Error("Invalid move: " + strmove);
			}
		    }
		    dbg('[AI] applied other move');
		    var result = searchChess(state2, initDepth);
		    dbg('[AI] got initial result');
		    if(result[0] === null) {
			throw new Error("Game is already over.");
		    }
		    if(result[1] === -Infinity) { 
			dbg('[AI] opponent has winning strategy');
			//opponent has a winning strategy
			//search at progressively lower depths
			//the logic is that, if the depth is lower, you assume
			//that the opponent is stupider.  You want a move that 
			//will avoid defeat for the smartest opponent that you 
			//can avoid defeat from.
			for(var depth2 = initDepth - 1; depth2 > 0; depth2--) {
			    result = searchChess(state2, depth2);
			    dbg('[AI] searched at depth' + depth2);
			    //if stupider opponent sees no winning strategy
			    if(result[1] !== -Infinity) {
				break;
			    }
			}
		    } else if(result[1] === Infinity) { 
			dbg('[AI] winning strategy detected');
			//you have a winning strategy
			//search at progressively lower depths
			//you want the fastest victory; that is, 
			//one that can be reached in the fewest steps
			for(var depth2 = initDepth - 1; depth2 > 0; depth2--) {
			    var result2 = searchChess(state2, depth2);
			    dbg('[AI] searched at depth ' + depth2);
			    //if you don't have a winning strategy at this depth
			    //use your move from the previous, higher depth
			    if(result2[1] !== Infinity) {
				break;
			    }
			    result = result2;
			}
		    }
		    var move = result[0][0];
		    dbg('[AI] got move: ' + move);
		    state = state2.applyMove(move)
		    dbg('[AI] applied move to internal state');
		    return moveToString(move);
		} catch(ex) {
		    dbg('[AI error] ' + ex.stack);
		    return null;
		}
	    }
	}
    }();


	    


    
