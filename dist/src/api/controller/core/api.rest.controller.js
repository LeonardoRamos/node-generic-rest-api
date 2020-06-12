'use strict';

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _joi = require('joi');

var _joi2 = _interopRequireDefault(_joi);

var _httpStatus = require('http-status');

var _httpStatus2 = _interopRequireDefault(_httpStatus);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _asyncToGenerator(fn) { return function () { var gen = fn.apply(this, arguments); return new Promise(function (resolve, reject) { function step(key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { return Promise.resolve(value).then(function (value) { step("next", value); }, function (err) { step("throw", err); }); } } return step("next"); }); }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

module.exports = function () {
    function ApiRestController(apiService) {
        _classCallCheck(this, ApiRestController);

        this.apiService = apiService;

        this.getBySlug = this.getBySlug.bind(this);
        this.insert = this.insert.bind(this);
        this.update = this.update.bind(this);
        this.list = this.list.bind(this);
        this.remove = this.remove.bind(this);
    }

    _createClass(ApiRestController, [{
        key: 'getBySlug',
        value: function () {
            var _ref = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee(req, res, next, slug) {
                var entityFound, e;
                return regeneratorRuntime.wrap(function _callee$(_context) {
                    while (1) {
                        switch (_context.prev = _context.next) {
                            case 0:
                                _context.prev = 0;
                                _context.next = 3;
                                return this.apiService.findBySlug(slug);

                            case 3:
                                entityFound = _context.sent;

                                if (entityFound) {
                                    _context.next = 8;
                                    break;
                                }

                                e = new Error('Entity not found');

                                e.status = _httpStatus2.default.NOT_FOUND;
                                return _context.abrupt('return', next(e));

                            case 8:

                                req.entity = entityFound;

                                return _context.abrupt('return', next());

                            case 12:
                                _context.prev = 12;
                                _context.t0 = _context['catch'](0);
                                return _context.abrupt('return', next(_context.t0));

                            case 15:
                            case 'end':
                                return _context.stop();
                        }
                    }
                }, _callee, this, [[0, 12]]);
            }));

            function getBySlug(_x, _x2, _x3, _x4) {
                return _ref.apply(this, arguments);
            }

            return getBySlug;
        }()
    }, {
        key: 'get',
        value: function get(req, res) {
            return res.json(req.entity);
        }
    }, {
        key: 'insert',
        value: function insert(req, res, next) {
            return this.apiService.save(req.body).then(function (savedEntity) {
                return res.json(savedEntity);
            }).catch(function (e) {
                return next(e);
            });
        }
    }, {
        key: 'update',
        value: function update(req, res, next) {
            var entity = _extends({}, req.entity, req.body);

            return this.apiService.update(entity).then(function (savedEntity) {
                return res.json(savedEntity);
            }).catch(function (e) {
                return next(e);
            });
        }
    }, {
        key: 'list',
        value: function list(req, res, next) {
            return this.apiService.findAll(req).then(function (entities) {
                return res.json(entities);
            }).catch(function (e) {
                return next(e);
            });
        }
    }, {
        key: 'remove',
        value: function remove(req, res, next) {
            return this.apiService.delete(req.entity).then(function () {
                return res.status(204);
            }).catch(function (e) {
                return next(e);
            });
        }
    }, {
        key: 'paramValidation',
        value: function paramValidation() {
            return {
                insert: {
                    body: {}
                },
                update: {
                    body: {},
                    params: {
                        slug: _joi2.default.string().hex().required()
                    }
                }
            };
        }
    }]);

    return ApiRestController;
}();
//# sourceMappingURL=api.rest.controller.js.map
