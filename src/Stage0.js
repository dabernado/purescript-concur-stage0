const reconcile = require('stage0/reconcile').reconcile
const h = require('stage0').h

exports._reconcile = (parent, renderedValues, newValues) => reconcile(parent, renderedValues, newValues, s => h(s))
exports.outerHTML = (node) => node.outerHTML
