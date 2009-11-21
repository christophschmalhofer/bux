
var whoType = { Empty:"_", Me:"o", You:"x"};

var board = [
    [whoType.Empty,whoType.Empty,whoType.Empty],
    [whoType.Empty,whoType.Empty,whoType.Empty],
    [whoType.Empty,whoType.Empty,whoType.Empty]];



function cellClicked(e) {
    $("#startButton").remove();
    var lineColumn = e.target.id.replace(/cell/, "");
    setWho(lineColumn.split(""), whoType.You, board);
    var winner = findWinner(board);
    if (winner) {
	gameOver ("Winner: " + winner);
	return; 
    }
    if (!emptyCells(board).length) {
	gameOver("Remis");
	return;
    }
    renderBoard();
    computeMove(whoType.Me);
}    

function boardActive() {
    renderBoard();
    $("td[id^=cell]").click(cellClicked).removeClass("Computing").addClass("YourTurn");    
}


$().ready(boardActive);


function aiStart(event) {
    computeMove(whoType.Me);
    $("#startButton").remove();
}



function gameOver(result) {
    println(result);
    $("td[id^=cell]").unbind("click").removeClass("YourTurn").removeClass("Computing").addClass("GameOver");
    renderBoard();
}


function println(x) {
    $("#out").append(x).append("<br>");
}

function print(x) {
    $("#out").append(x);
}

var winnerTriples = [
    [[0,0],[0,1],[0,2]],
    [[1,0],[1,1],[1,2]],
    [[2,0],[2,1],[2,2]],

    [[0,0],[1,0],[2,0]],
    [[0,1],[1,1],[2,1]],
    [[0,2],[1,2],[2,2]],

    [[0,0],[1,1],[2,2]],
    [[0,2],[1,1],[2,0]]
];


function getOtherPlayer(who) {
    return whoType.Me == who ? whoType.You : whoType.Me;
}

function computeMove(who) {

    function reallyCompute() {
	var emptyCellsVal = emptyCells(board);

	if(!emptyCellsVal.length) {
	    return;
	}

	var bestMove = -100;

	var utilityToPositions = [];

	function addPosition(utility, position) {
	    if(!utilityToPositions[utility]) {
		utilityToPositions[utility] = [];
	    }
	    utilityToPositions[utility].push(position);
	}

	//Optimierung für ersten Zug, sonst rechnet Javascript zu lange
	if (emptyCellsVal.length == 9) {
	    utilityToPositions[bestMove] = emptyCellsVal;
	} else {
	    for(var i = 0; i < emptyCellsVal.length; ++i) {
		var position = emptyCellsVal[i];
		var clone = cloneArray(board);
		setWho(position, who, clone);
		var utilityVal = utility(who, clone, getOtherPlayer(who), 1) ;
		addPosition(utilityVal, position);
		if (utilityVal >= bestMove) {
		    bestMove = utilityVal;
		}
	    }
	}
	var bestMoves = utilityToPositions[bestMove];
	var randomBestMove = bestMoves[Math.floor(Math.random() * bestMoves.length)];
	setWho(randomBestMove, who, board);

	var winner = findWinner(board);
	if (winner) {
	    gameOver ("Winner: " + winner);
	    return; 
	}
	if (!emptyCells(board).length) {
	    gameOver("Remis");
	    return;
	}
	boardActive();
    }
    $("td[id^=cell]").unbind("click").removeClass("YourTurn").addClass("Computing");
    setTimeout(reallyCompute, 0);
}


// das ist jetzt ein richtiger Minimax
// ein schneller Gewinn (depth klein) wird höher bewertet
function utility(who, field, mover, depth) {
    var winner = findWinner(field);
    if(winner) {
	return winner == who ? (20 - depth) : (-20 + depth);
    }
    if (!emptyCells(field).length) {
	return 0;
    }
    
    var foldedUtility;
    
    function foldUtility(recursedUtility) {
	if (foldedUtility === undefined) {
	    foldedUtility = recursedUtility;
	    return;
	}
	if( who == mover) {
	    foldedUtility = Math.max(foldedUtility, recursedUtility);
	} else {
	    foldedUtility = Math.min(foldedUtility, recursedUtility);
	}
    }

    var emptyCellsVal = emptyCells(field);
    for(var i = 0; i < emptyCellsVal.length; ++i) {
	var position = emptyCellsVal[i];
	var updatedField = cloneArray(field);
	setWho(position, mover, updatedField);
	foldUtility(utility(who, updatedField, getOtherPlayer(mover), depth + 1));
    }
    return foldedUtility;
}


function emptyCells(field) {
    var cells = [];
    for(var row = 0; row < field.length; ++row) {
	var line = field[row];
	for(var column = 0; column < line.length; ++column) {
	    if( getWho([row, column], field) == whoType.Empty) {
		cells.push([row, column]);
	    }
	}
    }
    return cells;
}

function cloneArray(array) {
    var clone = array.concat();
    for(var i=0; i < clone.length; ++i) {
	clone[i] = clone[i].concat();
    }
    return clone;
}

function toPos(input) {
    return input.split(",");
}

function setWho(position, who, field) {
    field[position[0]][position[1]] = who;
}

function makeFieldWhoFunction(field) {
    return function(position) {
	return getWho(position, field);
    };
}

function getWho(position, field) {
    return field[position[0]][position[1]];
}

function renderBoard() {
    renderField(board);
}

function renderField(field) {
    for (var row = 0; row < field.length; ++row) {
	for(var column = 0; column < field[0].length; ++column) {
	    $("#cell" + row + column).html( field[row][column]);
	}
    }
}


function findWinner(field) {
    for(var tripleIndex = 0; tripleIndex < winnerTriples.length; ++tripleIndex) {
	var triple = winnerTriples[tripleIndex];
	var winner = tryTriple();
	if( winner) { 
	    return winner;
	}
    }

    function tryTriple() {
	var who = makeFieldWhoFunction(field);
	var triple0 = who(triple[0]);
	var triple1 = who(triple[1]);
	if (triple0 != whoType.Empty && triple0 == triple1 && triple1 == who(triple[2])) {
	    return triple0;
	}
    }
}

