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
const DB_OPTS = { useNewUrlParser: true, useUnifiedTopology: true };

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

exports.handler = (event, context, callback) => {
  let cache
  try {
    cache = JSON.parse(event.body);
  } catch (error) {
    callback(error, {
      statusCode: 400,
      body: `could not parse ${ event.body }`
    });
    return;
  }

  try {
    MongoClient.connect(DB_URL, DB_OPTS, (error, client) => {
      if(error) {
          throw error;
      }
      const database = client.db(DB_NAME);
      const collection = database.collection('caches');
      console.log(`Connected to "${ DB_NAME}"!`);
      const decoration = R.pipe(
        R.ifElse(isRootCache, R.always(''), R.pipe(
          hash,
          humanize,
          R.replace(/ /g, '-'),
          R.toLower
        )),
        R.set(R.lensProp('key'), R.__, {})
      )(cache);

      console.log('hunting', decoration);

      collection.findOne(decoration, (findError, doc) => {
        if (doc) {
          callback(null, {
            statusCode: 200,
            body: JSON.stringify(doc)
          });
        } else {
          Object.assign(cache, decoration);

          collection.insertOne(cache, (insertError, result) => {
            if (result && result.ops && result.ops[0]) {
              callback(null, {
                statusCode: 200,
                body: JSON.stringify(result.ops[0])
              });
            } else {
              callback({
                statusCode: 500,
                body: JSON.stringify({
                  insertResult: result,
                  insertError,
                  findError
                })
              });
            }
          });
        }
      });
    });
  } catch (err) {
    callback({
      statusCode: 500,
      body: err.toString()
    });
  }
}
