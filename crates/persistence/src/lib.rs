#[cfg(test)]
pub mod test;

#[cfg(feature = "skytable")]
use skytable::actions::Actions;
#[cfg(feature = "skytable")]
use skytable::{Connection, SkyResult};

#[cfg(feature = "skytable")]
///
/// save the key-value table into skytable
///
/// # Errors
/// return () if `skytable` cannot connect
pub fn save_table(key: String, value: Vec<String>) -> SkyResult<()> {
    let mut con = Connection::new("127.0.0.1", 2003)?;
    con.del(key.clone())?;
    if let Ok(v) = serde_json::to_string(&value) {
        con.set(key.clone(), v)?;
    }
    value
        .into_iter()
        .for_each(|hash| match con.get::<String>(&hash) {
            Ok(_) => {
                con.update(&hash, key.clone()).ok();
            }
            Err(_) => {
                con.set(&hash, key.clone()).ok();
            }
        });
    Ok(())
}

#[cfg(feature = "redis")]
extern crate redis;
#[cfg(feature = "redis")]
use redis::Commands;
///
/// save the key-value table into redis
///
/// # Errors
/// return () if `redis` cannot connect
#[cfg(feature = "redis")]
pub fn save_table(key: String, value: Vec<String>) -> redis::RedisResult<()> {
    let client = redis::Client::open("redis://127.0.0.1/")?;
    let mut con = client.get_connection()?;
    con.del(key.clone())?;
    if let Ok(v) = serde_json::to_string(&value) {
        con.set(key.clone(), v)?;
    }
    value.into_iter().for_each(|hash| {
        con.set::<String, String, ()>(hash, key.clone()).ok();
    });
    Ok(())
}

///
/// update the value set with a new element
///
/// # Errors
/// return error if `redis` cannot connect or the element already exists
///
#[cfg(feature = "redis")]
pub fn update_set(key: String, value: String) -> redis::RedisResult<bool> {
    let client = redis::Client::open("redis://127.0.0.1/")?;
    let mut con = client.get_connection()?;
    let exists: bool = con.sismember(key.clone(), value.clone())?;
    if ! exists {
        con.sadd(key, value)?;
    }
    Ok(!exists)
}

use std::collections::BTreeMap;
use warning::Warning;
use serde::Serialize;

///
/// update the value set with a possible new element
///
/// # Errors
/// return () if `redis` cannot connect
/// # Panics
/// when the values k and v cannot be serialized into JSON format
#[cfg(feature = "redis")]
pub fn save_map<K,V>(key: String, map: BTreeMap<K,V>) -> redis::RedisResult<()> 
where K: Serialize, V: Serialize
{

    let client = redis::Client::open("redis://127.0.0.1/")?;
    let mut con = client.get_connection()?;
    map.iter().for_each(|(k, v)| {
        let k = serde_json::to_string(&k).unwrap();
        let v = serde_json::to_string(&v).unwrap();
        let k = k.replace("\"", "");
        con.set::<String, String, ()>(format!("{key}->{k}"), v).ok();
    });
    Ok(())
}

///
/// update the value set with a possible new element
///
/// # Errors
/// return () if `redis` cannot connect
#[cfg(feature = "redis")]
pub fn save_value(key: String, value: String) -> redis::RedisResult<()> {
    let client = redis::Client::open("redis://127.0.0.1/")?;
    let mut con = client.get_connection()?;
    con.set::<String, String, ()>(key, value).ok();
    Ok(())
}

///
/// load and merge the BTreeMaps from $project->$hash->$file
///
/// # Errors
/// return () if `redis` cannot connect
#[cfg(feature = "redis")]
pub fn load_map() -> BTreeMap<String,Vec<Warning>> {
    let mut map: BTreeMap<String, Vec<Warning>> = BTreeMap::new();
    if let Ok(client) = redis::Client::open("redis://127.0.0.1/") {
        if let Ok(mut con) = client.get_connection() {
            let projects: Vec<String> = con.smembers("projects").unwrap();
            projects.iter().for_each(|project| {
                let url: String = con.get(project).unwrap();
                let n: String = con.get(format!("{url}->revisions")).unwrap();
                let n = n.replace('"', "");
                let n = n.parse::<usize>().unwrap();
                for i in 1..n {
                    let keys: Vec<String> = con.keys(format!("{url}->{i:08}->warning->*")).unwrap();
                    keys.iter().for_each(|k| {
                        let values_string: String = con.get(k).unwrap();
                        if let Ok(w) = serde_json::from_str::<Vec<Warning>>(&values_string) {
                            map.insert(k.clone(), w);              
                        }
                    });
                }
            });
        }
    }
    map
}

///
/// load the LOC from $project->$hash->loc
///
/// # Errors
/// return () if `redis` cannot connect
#[cfg(feature = "redis")]
pub fn load_loc_map() -> BTreeMap<String,usize> {
    let mut map: BTreeMap<String,usize> = BTreeMap::new();
    if let Ok(client) = redis::Client::open("redis://127.0.0.1/") {
        if let Ok(mut con) = client.get_connection() {
            let projects: Vec<String> = con.smembers("projects").unwrap();
            projects.iter().for_each(|project| {
                let url: String = con.get(project).unwrap();
                let n: String = con.get(format!("{url}->revisions")).unwrap();
                let n = n.replace('"', "");
                let n = n.parse::<usize>().unwrap();
                for i in 1..n {
                    let k = format!("{url}->{i:08}->loc");
                    let loc: String = con.get(&k).unwrap();
                    let loc = loc.replace('"', "");
                    let loc = loc.parse::<usize>().unwrap();
                    map.insert(k.clone(), loc);              
                }
            });
        }
    }
    map
}

use function::functions;

///
/// save the functions into $project->$hash->$file->$start_byte->$start_line->$end_line
///
/// # Errors
/// return () if `redis` cannot connect
#[cfg(feature = "redis")]
pub fn save_functions() {
    if let Ok(client) = redis::Client::open("redis://127.0.0.1/") {
        if let Ok(mut con) = client.get_connection() {
            let projects: Vec<String> = con.smembers("projects").unwrap();
            projects.iter().for_each(|project| {
                let hashes_string: String = con.get(project).unwrap();
                if let Ok(hashes) = serde_json::from_str::<Vec<String>>(&hashes_string) {
                    let mut i: i32 = 0;
                    hashes.iter().for_each(|_| {
                        i += 1;
                        let keys: Vec<String> = con.keys(format!("{project}->{i:08}->*")).unwrap();
                        keys.iter().for_each(|k| {
                            let v = k.split("->").collect::<Vec<&str>>();
                            let _fn_map = functions(v[1].to_string(), v[0].to_string(), v[2].to_string());
                        });
                    });
                }
            });
        }
    }
}
