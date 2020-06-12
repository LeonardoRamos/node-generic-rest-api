import bcrypt from 'bcrypt';
import ApiRestService from './core/api.rest.service';
import db from '../config/sequelize';
const User = db.User;

module.exports = class UserService extends ApiRestService {
    
    constructor() {
        super(User);
    }

    async findByEmail(email) {
        return await User.findOne({ 
            where: { 
                email 
            } 
        });
    }

    save(user) {
        user.password = bcrypt.hashSync(user.password, user.password.length);
        return super.save(user);
    }

    update(user) {
        user.password = bcrypt.hashSync(user.password, user.password.length);
        return super.update(user);
    }

}