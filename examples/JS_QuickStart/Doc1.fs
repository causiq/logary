import Logary, { build, /* send */ getLogger, defaultTarget, Message } from 'logary'
// Supply this for your own user
const user = { id: 'abc', name: 'A B', email: 'a.b@example.com' } 

// Where to send logs?
const target = defaultTarget; // OR:

// ALTERNATIVELY: You can have one of these globally per application.
// const filter = next => req => next(req); // No infrastructure/non-functional requirements on requests

// A CUSTOM TARGET (uncomment send, filter, above):
// const target = Targets.logaryService({
//   path: 'http://localhost:10001/i/logary',
//   send: send(), // OR:
//   send: filter(send())
// })

// This instance is configured for the example user:
// You can have one of these prepared when the user logs in, in the user state store.
const logary = Logary(user, target, "MyWebApp");

// With 'build' we get a "live" function that can send to a target/server, use it to log
// You can have one of these in each of your module "screens"/parents
const sendMessage = build(logary, getLogger(logary, "MyModule"))

// Send a message!
// You can have one of these whereever you need to track stuff!
sendMessage(Message.event("App started")).subscribe()