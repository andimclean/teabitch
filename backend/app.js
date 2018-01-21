var express = require('express');
var path = require('path');
var favicon = require('serve-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');
var sassMiddleware = require('node-sass-middleware');
var moment = require('moment');

var index = require('./routes/index');
var users = require('./routes/users');

var app = express();

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'ejs');

// uncomment after placing your favicon in /public
//app.use(favicon(path.join(__dirname, 'public', 'favicon.ico')));
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded({ extended: false }));
app.use(cookieParser());
app.use(sassMiddleware({
  src: path.join(__dirname, 'public'),
  dest: path.join(__dirname, 'public'),
  indentedSyntax: true, // true = .sass and false = .scss
  sourceMap: true
}));
app.use(express.static(path.join(__dirname, 'public')));

app.use('/', index);

// catch 404 and forward to error handler
app.use(function(req, res, next) {
  var err = new Error('Not Found');
  err.status = 404;
  next(err);
});

// error handler
app.use(function(err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render the error page
  res.status(err.status || 500);
  res.render('error');
});

var tearooms = {};
getid = () => `${(Math.random() * 100000000)}`;
mapmember = (m) => (m ? {id: m.teaid, name: m.membername} : null);

function findOrCreateTearoom(name, io) {
  if (!tearooms[name]) {
    tearooms[name] = {
      name: name,
      members: new Set(),
      wantingtea: new Set(),
      roundendsat: null,
      broadcast: () => io.to(name)
    };
  }

  return tearooms[name];
}



function addMemberToTearoom(tearoom, socket) {
  if (!socket.teaid) { socket.teaid = getid(); }

  tearoom.members.add(socket);
  socket.join(tearoom.name);

  socket.emit('yourid', mapmember(socket));
  tearoom.broadcast().emit('joined', {members: [...tearoom.members.values()].map(mapmember)});

  if (tearoom.roundendsat) {
    tearoom.broadcast().emit('roundstarted', {timeleft: getMillisecondsToTime(tearoom.roundendsat.endtime)})
    tearoom.broadcast().emit('inround', {wantingtea: [...tearoom.wantingtea.values()].map(mapmember), timeleft: getMillisecondsToTime(tearoom.roundendsat.endtime)})
  }
}

function startTearoomRound(tearoom) {
  if (tearoom && !tearoom.roundendsat) {
    let endtime = moment().add(0.25, 'm');

    tearoom.roundendsat = {
      timer: setTimeout(getEndRoundHandler(tearoom), getMillisecondsToTime(endtime)),
      endtime: endtime
    };

    tearoom.broadcast().emit('roundstarted', {timeleft: getMillisecondsToTime(tearoom.roundendsat.endtime)})
  }
}

function randomNumberBetween0andX(x) {
  return Math.floor(Math.random() * x);
}

function getMillisecondsToTime(targetTime) {
  return Math.max(moment.duration(targetTime.diff(moment())).asMilliseconds(), 0);
}

function getEndRoundHandler(tearoom) {
  return () => {

    // choose tea maker
    var teamakerIndex = randomNumberBetween0andX(tearoom.wantingtea.size);
    var teamaker = [...tearoom.wantingtea][teamakerIndex];
    var teafor = [...tearoom.wantingtea].filter(m => m != teamaker).map(mapmember);

    // tell the room who's making tea
    tearoom.broadcast().emit('roundcomplete', {teamaker: mapmember(teamaker), teafor: teafor});

    tearoom.roundendsat = null;
    tearoom.wantingtea.clear();
  }
}

function addToTearound(tearoom, member) {
  if (tearoom && tearoom.roundendsat && tearoom.members.has(member)) {
    tearoom.wantingtea.add(member);

    tearoom.broadcast().emit('inround', {wantingtea: [...tearoom.wantingtea.values()].map(mapmember), timeleft: getMillisecondsToTime(tearoom.roundendsat.endtime)})
  }
}

function removeFromTeaRound(tearoom, member) {
  if (tearoom && member && tearoom.wantingtea.has(member)) {
    tearoom.wantingtea.delete(member);
    tearoom.broadcast().emit('inround', {wantingtea: [...tearoom.wantingtea.values()].map(mapmember), timeleft: getMillisecondsToTime(tearoom.roundendsat.endtime)})
  }
}

function removeMemberFromTearoom(tearoom, member) {
  if (tearoom && member) {
    tearoom.members.delete(member);
    tearoom.broadcast().emit('joined', {members: [...tearoom.members.values()].map(mapmember)});
  }
}

app.socket = (io) => {
  io.set('origins', '*:*');

  io.on('connection', (socket) => {
    var tearoom = null;

    socket.membername = null;

    socket.on('join', (data) => {
      if (tearoom) { removeMemberFromTearoom(tearoom, socket); }

      tearoom = findOrCreateTearoom(data.roomname, io);
      socket.membername = data.membername;
      addMemberToTearoom(tearoom, socket);
    });

    socket.on('wanttea', (data) => {
      startTearoomRound(tearoom);

      addToTearound(tearoom, socket);
    });

    socket.on('notea', (data) => {
      removeFromTeaRound(tearoom, socket);
    });

    socket.on('disconnect', () => {
      removeFromTeaRound(tearoom, socket);
      removeMemberFromTearoom(tearoom, socket);
    });

    console.log('a user connected', socket.id);
  });
  console.log('Doing io things');
}

module.exports = app;
