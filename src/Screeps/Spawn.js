"use strict";

exports.createCreepImpl = function(structure){
    return function(parts){
        return function(name){
            return function(opts){
                return function(){
                    if (name === "") { name = Game.time; }
                    return structure.spawnCreep(parts, name, opts);
                }
            }
        }
    }
}
