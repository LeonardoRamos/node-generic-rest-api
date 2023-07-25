import bcrypt from 'bcrypt';
import { ApiRestService, db } from 'generic-rest-express-lib';

const User = db.User;

module.exports = class UserService extends ApiRestService {
    
    constructor() {
        super(User);

        this.save = this.save.bind(this);
    }

    async findByEmail(email) {
        return await User.findOne({ 
            where: { email } 
        });
    }

    save(user) {
        user.password = bcrypt.hashSync(user.password, user.password.length);
        return super.save(user);
    }

}
