function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':3000' + path;

    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

function warning(w) {
    $('#warnings').append(w);
}

var ws = createWebSocket('/');
var user;
var game;
var started = false;
var yourturn = false;
var max = 6;
var games = [];
var state = new Array(max);
for (i=0; i<max; i++)
    state[i] = 0;

function refreshGames() {
    $('#game').html('');
    $('#game').append(new Option("New Game", -1, true, false));
    for(i in games) {
        $('#game').append(new Option(games[i], i, false, false));
    }
}

function onMessage(event) {
    if(event.data.match('^MOVE: ')) {
        var str = event.data.replace(/^MOVE: /, '').split(',');
        if(str[1] == user) {
            set(str[0], "blue");
        } else
            set(str[0], "red");
            yourturn = !yourturn;
        }
    } else if(event.data.match('^MESSAGE: ')) {
        var str = event.data.replace(/^MESSAGE: /, '');
        var p = $(document.createElement('p')).text(str);
        $('#messages').append(p);
        $('#messages').animate({scrollTop: $('#messages')[0].scrollHeight});
    } else if(event.data.match('^GAMES: ')) {
        var str = event.data.replace(/^GAMES: /, '');
        games = str.split(",").filter(function(e){return e});
        refreshGames();
    } else if(event.data.match('FIRST')) {
        yourturn = true;
    } else if(event.data.match('SECOND')) {
        yourturn = false;
    } else if(event.data.match('STARTED')) {
        started = true;
    } else {
        warning(event.data);
        ws.close();
    }
}

function play(column) {
    if(column < max && yourturn && started) {
        ws.send("MOVE: " + column + "," + user);
        yourturn = !yourturn;
    }
}

function set(column, color) {
    document.getElementById('field' + column + state[column]).style.color = color;
    state[column]++;
}

$(document).ready(function () {
    ws.onopen = function() {
        ws.send("HI");
    };

    ws.onmessage = function(event) {
        if(event.data.match('^GAMES: ')) {
            var str = event.data.replace(/^GAMES: /, '');
            games = str.split(",").filter(function(e){return e});
            refreshGames();
        }
    }

    $('#join-form').submit(function () {
        $('#warnings').html('');
        user = $('#user').val();
        game = $('#game').val();
        name = $('#name').val();

        // register with the server
        ws.send("(\"" + user + "\", " + game + ",\"" + name + "\")");
        ws.onmessage = function(event) {
            if(event.data.match('^ERROR: ')) {
                warning(event.data);
            } else if(event.data.match('OK')) {
                // register real message listener
                ws.onmessage = onMessage;

                // generating play buttons
                var buttons = "";
                for (i=0; i<max; i++) {
                    buttons = buttons + "<td class=\"toss-button\" onClick=\"play(" + i + ")\">&#x25bc;</td>";
                }
                $('#playground').append("<tr>" + buttons + "</tr>");

                // generating playground
                for (i=max-1; i>=0; i--) {
                    var cols="";
                    for (j=0; j<max; j++) {
                        var id = j + "" + i;
                        cols = cols + "<td id=\"field" + id + "\">&#x25cf;</td>";
                    }
                    $('#playground').append("<tr>" + cols + "</tr>");
                }

                $('#join-section').hide();
                $('#chat-section').show();
                $('#game-section').show();
                $('#message-form').submit(function () {
                    var text = $('#text').val();
                    ws.send("MESSAGE: " + user + ": " + text);
                    $('#text').val('');
                    return false;
                });
            } else {
                warning("This should never happen!");
            }
        }

        return false;
    });
});
