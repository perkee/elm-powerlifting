const { MongoClient } = require("mongodb");

const DB_SRVR = 'cluster0-ue7nl.azure.mongodb.net/testtest?retryWrites=true';
const DB_USER = 'root';
const DB_PASS = process.env.DB_PASS;
const DB_NAME = "scratch-powerlifting";
const DB_URL = `mongodb+srv://${DB_USER}:${DB_PASS}@${DB_SRVR}/${DB_NAME}`;
const DB_OPTS = { useNewUrlParser: true, useUnifiedTopology: true };

exports.handler = (event, context, callback) => {
  try {
    console.log('trying to connect to ', DB_URL, DB_OPTS);
    MongoClient.connect(DB_URL, DB_OPTS, (error, client) => {
      if(error) {
        console.error('error connecting', error);
        client.close();
        callback(null, {
          statusCode: 500,
          body: JSON.stringify({ error })
        });
      }
      const database = client.db(DB_NAME);
      const collection = database.collection('caches');
      console.log(`Connected to "${ DB_NAME}"!`);
      const key = event.queryStringParameters && event.queryStringParameters.key

      console.log('hunting', { key });

      collection.findOne({ key }, (findError, doc) => {
        if (doc) {
          client.close();
          callback(null, {
            statusCode: 200,
            body: JSON.stringify(doc)
          });
        } else {
          client.close();
          callback(findError, {
            statusCode: 404,
            body: JSON.stringify(findError)
          });
        }
      });
    });
  } catch (err) {
    client.close();
    callback({
      statusCode: 500,
      body: err.toString()
    });
  }
}
