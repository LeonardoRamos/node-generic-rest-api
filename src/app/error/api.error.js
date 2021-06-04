import httpStatus from 'http-status';

class ExtendableError extends Error {
    constructor(message, code, status, isPublic) {
        super(message);
        
        this.name = this.constructor.name;
        this.code = code;
        this.message = message;
        this.status = status;
        this.isPublic = isPublic;
        this.isOperational = true;
        
        Error.captureStackTrace(this, this.constructor.name);
    }
}

class ApiError extends ExtendableError {
    constructor(message, code, status = httpStatus.INTERNAL_SERVER_ERROR, isPublic = false) {
        super(message, code, status, isPublic);
    }
}

export default ApiError;
