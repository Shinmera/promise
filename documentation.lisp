#|
 This file is a part of promise
 (c) 2021 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.promise)

(docs:define-docs
  (function clear
    "Clear all known promises out of the system.

You should only use this if you have promises that are \"stuck\" and
you don't care if they are resolved or not.

See TICK-ALL")
  
  (type promise
    "Represents a promise for a future value.

A promise can be in four different states:

  :PENDING -- The promise should be resolved in the future.
  :SUCCESS -- The promise has successfully resolved and is done.
  :FAILURE -- The promise has failed to resolved properly and is done.
  :TIMEOUT -- The promise did not resolve in time and is done.

A promise can only be moved to one of the latter three states. Once
done, it cannot be \"re-armed\". Attempting to chain another handler
onto a promise that is done will cause the handler to fire immediately
or not at all, depending on the type of handler. In that case the
handler will be called synchronously (!) If a handler is attached to a
pending promise, the handler will be called asynchronously whenever
the promise moves to the requested state and TICK is called.

Handlers can be attached to a promise through AFTER.

A promise can be manually forced into one of its other states through
SUCCEED, FAIL, and TIMEOUT.

Once a promise has been constructed, it is registered globally so it
can be ticked through TICK-ALL. Once a promise has resolved and all
its handlers have been processed through TICK, it is deregistered
again, freeing it for collection.

Note that this global registration means that you MUST always resolve
a promise, or in the very least always set a lifetime, or the promise
will stick around in the image as garbage forever.

See STATE
See SUCCEED
See FAIL
See TIMEOUT
See MAKE
See PEND
See TICK
See TICK-ALL
See AFTER
See DONE-P")

  (function done-p
    "Returns whether the promise is \"done\".

A promise is considered done if it is not in the pending state,
meaning if it is in either success, failure, or timeout satte.

See PROMISE")
  
  (function state
    "Returns the current state of the promise.

See PROMISE")
  
  (function succeed
    "Moves the promise to the succeeded state if possible, using the given value.

If the promise is timed out, nothing happens. If the promise is
already failed or succeeded, an error is signalled.

Returns the promise.

See PROMISE")
  
  (function fail
    "Moves the promise to the failed state if possible, using the given error.

If the promise is timed out, nothing happens. If the promise is
already failed or succeeded, an error is signalled.

Returns the promise.

See PROMISE")
  
  (function timeout
    "Moves the promise to the timed-out state if possible.

If the promise is timed out, nothing happens. If the promise is
already failed or succeeded, an error is signalled.

Returns the promise.

See PROMISE")
  
  (function make
    "Creates a new promise.

The CONSTRUCTOR, if passed, should be a function of two arguments. The
two arguments are a succeed and fail function of one argument
respectively, which you should call once the promise should be
succeeded or failed, passing along the respective value if any.

Note that you can also force the returned promise into a state outside
of using the two functions passed into the constructor, by using the
manual state transition functions SUCCEED, FAIL, TIMEOUT.

The LIFETIME should be a positive REAL that determines the maximum
number of seconds the promise is considered \"live\" for. If the
promise is TICKed after LIFETIME seconds have passed, it will be moved
to the TIMEOUT state. Lifetimes are useful to avoid a computation
getting stuck on a promise that might never be fulfilled due to
reliability issues in the system.

See PROMISE")

  (function with-promise
    "Shorthand for MAKE with an initialisation function.

SUCCEED and FAIL are variable names bound to the respective functions
to manage the promise's state. The names are both bound as a variable
and bound as a function for convenience.

See MAKE")
  
  (function pend
    "Create a dummy promise.

This is the same as using MAKE. If SUCCESS or FAILURE are passed
respectively, the success/failure function is called immediately with
the given value from the argument.

See MAKE")
  
  (function tick
    "Ticks the promise to check its timeout or process handlers.

If the promise is done, all respective handlers for the promise's
state are called and popped off the promise's handler stack.
Afterwards the promise is removed from the global registry.

If the promise is pending, its timeout is checked against the given
TIME (which should be a universal-time timestamp) and if timed out, is
moved to the TIMEOUT state, then immediately proceeding as above.

See PROMISE
See TICK-ALL")
  
  (function tick-all
    "Calls TICK on all registered promises.

If there were any promises to tick, T is returned, otherwise NIL.

See PROMISE
See TICK")

  (function ensure-promise
    "Coerces the object into a PROMISE object, if possible.

Signals an error if not possible.

The user may add methods on this function to allow integration with
promise wrapper objects with the following restriction:

  (eq (ensure-promise #1) (ensure-promise #1#))

Meaning calling ENSURE-PROMISE on the identical argument again must
yield an identical returned PROMISE.

See PROMISE")
  
  (function after
    "Perform an action after the promise has reached a particular state.

Returns a new promise that encapsulates whether the attached handlers
have successfully run or not. If one of the handlers is called and
successfully returns, the new promise proceeds*. If a handler signals
an error, the new promise fails.

If the promise is already done, the respective handler function is
/still/ called asynchronously!

*When the new promise proceeds with a value from the handler, it acts
 differently depending on the returned value. If the value is another
 PROMISE, the new promise is linked to the returned promise, updating
 its state in accordance with the returned promise. If the value is
 any other type, the new promise is instead simply moved into its
 SUCCESS state.

The passed promise is coerced via ENSURE-PROMISE.

See ENSURE-PROMISE
See PROMISE
See THEN
See HANDLE
See FINALLY
See ALL
See ANY")
  
  (function then
    "Shorthand for attaching a success handler.

See AFTER")
  
  (function handle
    "Shorthand for attaching a failure handler.

If TYPE is passed, only invokes ON-FAILURE if the value is of TYPE.

See HANDLE")
  
  (function finally
    "Shorthand for attaching a handler that's called when the promise is done.

The passed function should take no arguments.

See HANDLE")
  
  (function all
    "Returns a promise that succeeds if all PROMISES succeed.

The passed promises are coerced via ENSURE-PROMISE.

See PROMISE")
  
  (function any
    "Returns a promise that succeeds if at least one PROMISE succeeds.

The passed promises are coerced via ENSURE-PROMISE.

See PROMISE")

  (function iterate
    "Create a promise that can iterate using the provided iteration functions.

This works as follows: START is used as the starting state of the
iteration. Then, until END-P on the current state returns T, promises
are generated. Each time calling CUR-FUN with the state to retrieve
the iteration element. And STEP-FUN with the state to retrieve the
successor state. FUNCTION is then called with the iteration
element. If FUNCTION returns a promise, the next iteration is chained
to the completion of that promise. If not, the next iteration is
chained immediately.

The promise returned by this function will only succeed once the
iteration has completed.

See PROMISE
See EACH")

  (function each
    "Iterate over a sequence, calling FUNCTION for each element.

If FUNCTION returns a promise, iteration is suspended until the
promise succeeds. Should FUNCTION ever error, or a promise it returns
fail, the iteration is cancelled.

Returns a promise that encapsulates whether the sequence has
successfully been iterated through.

See ITERATE
See DO-PROMISED")

  (function do-promised
    "Shorthand macro for EACH.

See EACH")

  (function do-times-promised
    "Same as DOTIMES, but with a promise for each iteration.

See ITERATE")
  
  (function ->
    "Shorthand macro to chain promise construction functions together.

Example:

  (-> (make)
      (after :success ...)
      (then (v) ...)
      (handle (e) ...)
      (finally ...))

See PROMISE")

  (function promise-handler-case
    "Convenience macro to handle different failures.

Each form in the body should be of the structure:

  (TYPE ARGLIST . BODY)

Similar to HANDLER-BIND. Each of the body forms is turned into a
promise that is attached to the primary promise.

The primary promise is returned.

See PROMISE
See HANDLE"))
