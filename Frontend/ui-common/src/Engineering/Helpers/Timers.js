import {
  callbackMapper
} from "presto-ui";

const JBridge = window.JBridge;

const activeTimers = {};

let activeTimerIds = [];

function instantGetTimer(fn, id, delay) {
  const timerId = setInterval(fn, delay, id);
  return timerId;
}

export const countDownImpl = function (countDownTime, id, interval, cb, action) {
  activeTimerIds.push(id);
  if (countDownTime < interval) interval = countDownTime;
  if (activeTimers[id] != undefined) {
    clearInterval(activeTimers[id].id);
  }
  const handler = function (keyId) {
    const timer = activeTimers[keyId];
    if (timer) {
      timer.time = timer.time - timer.timerInterval;
      if (timer.time <= 0) {
        clearInterval(timer.id);
        activeTimers[keyId] = undefined;
        delete activeTimers[keyId];
        cb(action(0)("EXPIRED")(keyId))();
      } else {
        cb(action(timer.time)("INPROGRESS")(keyId))();
      }
    }
  }
  const timerId = instantGetTimer(handler, id, interval * 1000);
  const timer = {
    time: countDownTime,
    id: timerId,
    timerInterval: parseInt(interval)
  }
  activeTimers[id] = timer;
  handler(id);
}

export const clearTimerWithId = function (id) {
  if (window.__OS == "IOS") {
    if (JBridge.clearTimerWithId) {
      JBridge.clearTimerWithId(id);
    } else {
      if (JBridge.clearCountDownTimer) {
        JBridge.clearCountDownTimer();
      }
      if (JBridge.clearCountUpTimer && id == "countUpTimerId") {
        window.JBridge.clearCountUpTimer();
      }
    }
  } else {
    if (activeTimers[id] != undefined) {
      clearInterval(activeTimers[id].id);
      activeTimers[id] = undefined;
      delete activeTimers[id];
    }
  }
  activeTimerIds = activeTimerIds.filter((value) => {
    return value != id
  });
}

function getTwoDigitsNumber(number) {
  return number >= 10 ? number : "0" + number.toString();
}

export const waitingCountdownTimerV2Impl = function (startingTime, interval, timerId, cb, action) {
  activeTimerIds.push(timerId);
  if (window.__OS == "IOS") {
    if (JBridge.startCountUpTimerV2) {
      const callbackIOS = callbackMapper.map(function (timerID, sec) {
        const minutes = getTwoDigitsNumber(Math.floor(sec / 60));
        const seconds = getTwoDigitsNumber(sec - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(timerID)(timeInMinutesFormat)(sec))();
      });
      JBridge.startCountUpTimerV2(startingTime.toString(), interval, timerId, callbackIOS);
    } else if (JBridge.startCountUpTimer) {
      const callbackIOS = callbackMapper.map(function (timerID, sec) {
        const minutes = getTwoDigitsNumber(Math.floor(sec / 60));
        const seconds = getTwoDigitsNumber(sec - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(timerID)(timeInMinutesFormat)(sec))();
      });
      JBridge.startCountUpTimer(startingTime.toString(), callbackIOS);
    }
  } else {
    if (activeTimers[timerId] != undefined) {
      clearInterval(activeTimers[timerId].id);
    }
    const handler = function (keyId) {
      const timer = activeTimers[keyId];
      if (timer) {
        timer.time = timer.time + timer.timerInterval;
        const minutes = getTwoDigitsNumber(Math.floor(timer.time / 60));
        const seconds = getTwoDigitsNumber(timer.time - minutes * 60);
        const timeInMinutesFormat = minutes + " : " + seconds;
        cb(action(keyId)(timeInMinutesFormat)(timer.time))();
      }
    }
    const timerID = instantGetTimer(handler, timerId, interval * 1000);
    const timer = {
      time: startingTime,
      id: timerID,
      timerInterval: parseInt(interval)
    }
    activeTimers[timerId] = timer;
    handler(timerId);
  }
}

export const startTimerWithTimeV2Impl = function (time, cdTimerId, interval, cb, action) {
  activeTimerIds.push(cdTimerId);
  if (JBridge.startCountDownTimerWithTimeV2) {
    const callback = callbackMapper.map(function (seconds, timerStatus, timerID) {
      cb(action(seconds)(timerStatus)(timerID))();
    });
    return JBridge.startCountDownTimerWithTimeV2(time, interval, cdTimerId, callback);
  }
  if (JBridge.startCountDownTimerWithTime) {
    const callback = callbackMapper.map(function (seconds, id, timerStatus, timerID) {
      cb(action(seconds)(timerStatus)(timerID))();
    });
    return JBridge.startCountDownTimerWithTime(time, interval, cdTimerId, callback);
  }
}

export const resetAllTimers = function () {
  activeTimerIds.forEach((value) => {
    clearTimerWithId(value);
  })
}
export const rideDurationTimerImpl = (startingTime, interval, timerId, cb, action) => {
  if (window.__OS == "IOS") {
    if (JBridge.startCountUpTimerV2) {
      const callbackIOS = callbackMapper.map(function (timerID, sec) {
        const mins = getTwoDigitsNumber(Math.floor(sec / 60));
        const hours = getTwoDigitsNumber(Math.floor(mins / 60));
        const minutes = getTwoDigitsNumber(Math.floor((mins - (hours * 60))));
        const timeInHHMMFormat = hours + " : " + minutes;
        cb(action(timerID)(timeInHHMMFormat)(mins))();
      });
      const updatedStartingTime = startingTime * 60;
      JBridge.startCountUpTimerV2(updatedStartingTime.toString(), interval, timerId, callbackIOS);
    }
    else if (JBridge.startCountUpTimer) {
      const callbackIOS = callbackMapper.map(function (timerID, sec) {
        const mins = getTwoDigitsNumber(Math.floor(sec / 60));
        const hours = getTwoDigitsNumber(Math.floor(mins / 60));
        const minutes = getTwoDigitsNumber(Math.floor((mins - (hours * 60))));
        const timeInHHMMFormat = hours + " : " + minutes;
        cb(action(timerID)(timeInHHMMFormat)(mins))();
      });
      JBridge.startCountUpTimer(startingTime.toString(), callbackIOS);
    }
  } else {
    if (activeTimers[timerId] != undefined) {
      clearInterval(activeTimers[timerId].id);
    }
    const handler = function (keyId) {
      const timer = activeTimers[keyId];
      if (timer) {
        timer.time = timer.time + timer.timerInterval;
        const mins = timer.time;
        // const mins = Math.floor(timer.time/60); // IF WE ARE STORING TIME IN SECONDS IN activeTimers ARRAY because of interval in seconds
        const hours = getTwoDigitsNumber(Math.floor(mins / 60));
        const minutes = getTwoDigitsNumber(Math.floor((mins - (hours * 60))));
        const timeInHHMMFormat = hours + " : " + minutes;
        cb(action(keyId)(timeInHHMMFormat)(timer.time))();
      }
    }
    const timerID = instantGetTimer(handler, timerId, interval * (60000));
    const timer = {
      time: startingTime,
      id: timerID,
      timerInterval: parseInt(interval)
    }
    activeTimers[timerId] = timer;
    handler(timerId);
  }
}