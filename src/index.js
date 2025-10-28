// noinspection JSUnresolvedReference

import { Elm } from './Main.elm';
import css from './../css/styles.css'

const initDb = {
    apiToken: null,
    apiRate: 30,
    running: true,
    profiles: {}
};

const getDatabase = (storage) => {
    const databaseStr = storage.getItem('torn-chain');
    if (databaseStr) {
        const database = JSON.parse(databaseStr);
        if (database) {
            return {
                ...initDb,
                ...database
            }
        }
    }
    return {
        ...initDb
    }
};

const saveDatabase = (database, storage) => {
    storage.setItem('torn-chain', JSON.stringify(database));
}

const createApp = (node, storage) => {
    const db = getDatabase(storage);
    const app = Elm.Main.init({
        node,
        flags: {
            apiToken: db.apiToken,
            apiRate: db.apiRate
        }
    });
    return app;
}


const startTornChainingApp = (node, storage) => {

    const app = createApp(node, storage);

    const send = (type, payload) => {
        setTimeout(function() {
            app.ports.receive.send({
                type: type,
                payload: payload
            });
        }, 0);
    };

    const bc = new BroadcastChannel("torn-chain");

    bc.onmessage = (event) => {
        app.ports.receive.send(event.data);
    };

    const broadcast = (type, payload) => {
        bc.postMessage({
            type: type,
            payload: payload
        });
    }

    const handleCommand = (cmd) => {
        const command = cmd.command;
        const payload = cmd.payload;
        let database = getDatabase(storage);
        switch (command) {
            case 'saveApiToken':
                database.apiToken = payload;
                saveDatabase(database, storage);
                break;
            case 'saveApiRate':
                database.apiRate = payload;
                saveDatabase(database, storage);
                break;
            case 'setRunning':
                database.running = payload;
                saveDatabase(database, storage);
                broadcast('running', payload)
                break;
            case 'loadPlayers':
                send('running', !!database.running);
                if (database.throttle) {
                    send('throttle', database.throttle);
                }
                Object.values(database.profiles).forEach(profile => {
                    send('player', profile);
                });
                break;
            case 'loadThrottle':
                if (database.throttle) {
                    send('throttle', database.throttle);
                }
                break;
            case 'savePlayer':
                database.profiles[payload.id] = payload;
                saveDatabase(database, storage);
                broadcast('player', payload)
                break;
            case 'saveThrottle':
                database.throttle = payload;
                saveDatabase(database, storage);
                broadcast('throttle', payload)
                break;
            case 'deletePlayer':
                delete database.profiles[payload];
                saveDatabase(database, storage);
                break;
            default:
                console.error("Received unknown command", cmd);
        }
    }

    app.ports.command.subscribe(handleCommand);
};

const content = document.createElement("div");
const app = document.createElement("div");

content.style.position = "fixed";
content.style.left = "0";
content.style.top = "0";
content.style.width = "330px";
content.style.border = "1px solid #ddd";
content.style.height = "100vh";
content.style.zIndex = "100000";
content.style.boxSizing = "border-box";
content.className = "content"

content.append(app);
document.body.append(content);

const st = document.createElement("style");
st.textContent = css;
document.head.appendChild(st);

startTornChainingApp(app, window.localStorage);