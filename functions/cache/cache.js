const objHash = require('object-hash');
const humanize = require('angry-purple-tiger');
const { MongoClient } = require("mongodb");
const R = require('ramda');
const { validate } = require('jsonschema');

const DB_SRVR = 'cluster0-ue7nl.azure.mongodb.net/testtest?retryWrites=true';
const DB_USER = 'root';
const DB_PASS = process.env.DB_PASS;
const DB_NAME = "scratch-powerlifting";
const DB_URL = `mongodb+srv://${DB_USER}:${DB_PASS}@${DB_SRVR}/${DB_NAME}`;
const DB_OPTS = { useNewUrlParser: true, useUnifiedTopology: true, poolSize: 10 };

const hash = x => objHash(x);
const isRootCache = cache => validate(cache, {
  type: "object",
  properties: {
    version: {
      type: "number"
    },
    feats: {
      type: "array",
      maxItems: 0
    }
  }
}).valid;

let mongoDbConnectionPool = null;

function getMongoDbConnection(uri) {
  if (
    mongoDbConnectionPool &&
    mongoDbConnectionPool.isConnected(DB_NAME)
  ) {
    console.log("Reusing the connection from pool");
    return Promise.resolve(mongoDbConnectionPool.db(DB_NAME));
  }
  console.log("Init the new connection pool");
  return MongoClient
    .connect(uri, DB_OPTS)
    .then(dbConnPool => {
      mongoDbConnectionPool = dbConnPool;
      console.log('made new connection pool');
      return mongoDbConnectionPool.db(DB_NAME);
    });
}

const prepareResponse = (result, err = null, statusCode = null, verbose = '') => {
  return err ? {
    statusCode,
    body : {
      err,
      verbose
    }
  } : {
    statusCode: 200,
    body: JSON.stringify(result)
  }
}

const post = (body, callback) => {
  let cache;

  try {
    cache = JSON.parse(body);
  } catch (error) {
    callback(error, {
      statusCode: 400,
      body: `could not parse ${ body }`
    });
    return;
  }

  const decoration = R.pipe(
    R.ifElse(isRootCache, R.always(''), R.pipe(
      hash,
      humanize,
      R.replace(/ /g, '-'),
      R.toLower
    )),
    R.set(R.lensProp('key'), R.__, {})
  )(cache);

  getMongoDbConnection(DB_URL)
    .then(dbConn => {
      console.log('hunting', decoration);
      const collection =  dbConn.collection('caches');

      collection.findOne(decoration).then((response) => {
        if (response) {
          console.log("found: ", response);
          callback(null, prepareResponse(response));
        } else {
          console.log("did not find: ", response);

          Object.assign(cache, decoration);

          collection.insertOne(cache)
            .then(r => {
              console.log("inserted: ", r && r.ops && r.ops[0]);

              callback(null, prepareResponse(r && r.ops && r.ops[0]))
            }).catch((err) => {
              console.log("did not insert: ", err);

              callback(prepareResponse(null, err));
            });
        }
      });
    })
    .catch((err) => {
      console.log("did not connect: ", err);

      callback(prepareResponse(null, err));
    });
}

const get = (params, callback) => {
  const { key } = params;

  getMongoDbConnection(DB_URL)
    .then(dbConn => {
      const collection = dbConn.collection('caches');

      collection.findOne({ key }, (findError, doc) => {
        if (doc) {
          callback(null, {
            statusCode: 200,
            body: JSON.stringify(doc)
          });
        } else {
          callback(findError, {
            statusCode: 404,
            body: JSON.stringify(findError)
          });
        }
      });
    })
}

exports.handler = (event, context, callback) => {
  context.callbackWaitsForEmptyEventLoop = false;
  console.log('full context', context);
  console.log('event', event);

  switch (event.httpMethod) {
    case 'POST':
      post(event.body, callback)
      break;

    case 'GET':
      get(event.queryStringParameters, callback)
      break;

    default:
      callback(error, {
        statusCode: 400,
        body: `cannot handle method ${ event.httpMethod }`
      });
      break;
  }
}
