package it.unipi.dsmt.fitconnect.util;

import it.unipi.dsmt.fitconnect.entities.MongoUser;
import org.springframework.core.convert.converter.Converter;
import org.springframework.data.convert.WritingConverter;
import org.springframework.data.mongodb.core.mapping.DocumentPointer;

@WritingConverter
public class UserReferenceConverter implements Converter<MongoUser, DocumentPointer<String>> {
    @Override
    public DocumentPointer<String> convert(MongoUser user) {
        return () -> user.getUsername();
    }
}
