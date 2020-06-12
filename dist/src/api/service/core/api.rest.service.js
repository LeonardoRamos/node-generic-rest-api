'use strict';

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _uuid = require('uuid');

var _sequelize = require('sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

var _query = require('../../helpers/query.builder');

var _query2 = _interopRequireDefault(_query);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _asyncToGenerator(fn) { return function () { var gen = fn.apply(this, arguments); return new Promise(function (resolve, reject) { function step(key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { return Promise.resolve(value).then(function (value) { step("next", value); }, function (err) { step("throw", err); }); } } return step("next"); }); }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

module.exports = function () {
    function ApiRestService(model) {
        _classCallCheck(this, ApiRestService);

        this.model = model;

        this.findBySlug = this.findBySlug.bind(this);
        this.save = this.save.bind(this);
        this.findAll = this.findAll.bind(this);
    }

    _createClass(ApiRestService, [{
        key: 'findBySlug',
        value: function () {
            var _ref = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee(slug) {
                return regeneratorRuntime.wrap(function _callee$(_context) {
                    while (1) {
                        switch (_context.prev = _context.next) {
                            case 0:
                                _context.next = 2;
                                return this.model.findOne({
                                    where: {
                                        slug: slug
                                    }
                                });

                            case 2:
                                return _context.abrupt('return', _context.sent);

                            case 3:
                            case 'end':
                                return _context.stop();
                        }
                    }
                }, _callee, this);
            }));

            function findBySlug(_x) {
                return _ref.apply(this, arguments);
            }

            return findBySlug;
        }()
    }, {
        key: 'findAll',
        value: function () {
            var _ref2 = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee2(req) {
                var query, result;
                return regeneratorRuntime.wrap(function _callee2$(_context2) {
                    while (1) {
                        switch (_context2.prev = _context2.next) {
                            case 0:
                                query = _query2.default.buildQuery(this.model, req.query);
                                _context2.next = 3;
                                return this.model.findAndCountAll(query);

                            case 3:
                                result = _context2.sent;
                                return _context2.abrupt('return', {
                                    records: result.rows,
                                    metadata: {
                                        totalCount: result.count,
                                        pageOffset: query.offset,
                                        pageSize: query.limit
                                    }
                                });

                            case 5:
                            case 'end':
                                return _context2.stop();
                        }
                    }
                }, _callee2, this);
            }));

            function findAll(_x2) {
                return _ref2.apply(this, arguments);
            }

            return findAll;
        }()
    }, {
        key: 'save',
        value: function () {
            var _ref3 = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee3(entity) {
                var entityModel;
                return regeneratorRuntime.wrap(function _callee3$(_context3) {
                    while (1) {
                        switch (_context3.prev = _context3.next) {
                            case 0:
                                entityModel = this.model.build(req.entity);


                                entityModel.slug = (0, _uuid.v4)().split('-').join('').toUpperCase();
                                entityModel.insertDate = _sequelize2.default.NOW;
                                entityModel.updateDate = entityModel.insertDate;

                                _context3.next = 6;
                                return entityModel.save();

                            case 6:
                                return _context3.abrupt('return', _context3.sent);

                            case 7:
                            case 'end':
                                return _context3.stop();
                        }
                    }
                }, _callee3, this);
            }));

            function save(_x3) {
                return _ref3.apply(this, arguments);
            }

            return save;
        }()
    }, {
        key: 'update',
        value: function () {
            var _ref4 = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee4(entityModel) {
                return regeneratorRuntime.wrap(function _callee4$(_context4) {
                    while (1) {
                        switch (_context4.prev = _context4.next) {
                            case 0:
                                entityModel.updateDate = _sequelize2.default.NOW;
                                _context4.next = 3;
                                return entity.save();

                            case 3:
                                return _context4.abrupt('return', _context4.sent);

                            case 4:
                            case 'end':
                                return _context4.stop();
                        }
                    }
                }, _callee4, this);
            }));

            function update(_x4) {
                return _ref4.apply(this, arguments);
            }

            return update;
        }()
    }, {
        key: 'delete',
        value: function () {
            var _ref5 = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee5(entity) {
                return regeneratorRuntime.wrap(function _callee5$(_context5) {
                    while (1) {
                        switch (_context5.prev = _context5.next) {
                            case 0:
                                _context5.next = 2;
                                return entity.destroy();

                            case 2:
                                return _context5.abrupt('return', _context5.sent);

                            case 3:
                            case 'end':
                                return _context5.stop();
                        }
                    }
                }, _callee5, this);
            }));

            function _delete(_x5) {
                return _ref5.apply(this, arguments);
            }

            return _delete;
        }()
    }]);

    return ApiRestService;
}();
//# sourceMappingURL=api.rest.service.js.map
