// The client-side config for the game.
var config = {
    p1 :  1,
    p2 : -1,
    nil:  0,

    p1Color : '#17e84b',
    p2Color : '#fa0530',
    nilColor: '#ffffff'
};

// The client-side state of the game.
var localState = {
    state   : [ 0, 0, 0, 0, 0, 0, 0, 0, 0 ],
    turn    : 1,
    over    : false,
    winner  : 0,
    canMove : true
};

// Performing some function with the state from the server.
function withState(fn) {
    $.ajax({
        url: '/api/pull/state',
        type: 'GET'
    }).done(fn);
}

// Redrawing the board with a given state.
function drawBoard() {
    for (var i = 0; i < 9; i++) {
        var lc;
        if (localState.state[i] == 0)
            lc = config.nilColor;
        else if (localState.state[i] == 1)
            lc = config.p1Color;
        else if (localState.state[i] == -1)
            lc = config.p2Color;

        $('span[data-idx=' + i.toString() + ']').css('background', lc);
    }
}

// Setting the initial state for the client.
function updateLocalState(state) {
    withState(function (data) {
        localState.state = data.state;
        localState.turn  = data.turn;
        localState.over  = data.over;
        localState.winner = data.winner;
        
        if (localState.turn != 1 || localState.over)
            localState.canMove = false;
        else
            localState.canMove = true;

        drawBoard();
    });
}

// Handling the response from a POST on a push route.
function handleOutput(data) {
    $('#messagebox').html(data['msg']);
    updateLocalState();
}

// Responding to a click from the user.
function respondClick() {
    if (localState.canMove) {
        var json = {
            row: parseInt($(this).attr('data-row')),
            col: parseInt($(this).attr('data-col'))
        };

        localState.canMove = false;
        $('#messagebox').html('AI is moving...');

        localState.state[parseInt($(this).attr('data-idx'))] = config.p1;
        drawBoard();

        $.ajax({
            url: '/api/push/move',
            type: 'POST',
            contentType: 'application/json;charset=UTF-8',
            data: JSON.stringify(json)
        }).done(handleOutput);
    }
}

// Responding to a click from the button reset.
function resetClick() {
    $.ajax({
        url: '/api/push/reset',
        type: 'POST'
    }).done(handleOutput);
}

// Stuff to run when the page has rendered.
$(document).ready(function () {
    updateLocalState();

    $('.tictacgrid').each(function() {
        $(this).click(respondClick);
    });

    $('#resetbutton').click(resetClick);
});

