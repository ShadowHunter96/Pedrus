package cz.bbn.cerberus.document.persistance.repository;

import cz.bbn.cerberus.document.persistance.entity.DocumentFileMongoEntity;
import org.springframework.data.mongodb.repository.MongoRepository;

import java.util.Optional;

public interface DocumentFileMongoRepository extends MongoRepository<DocumentFileMongoEntity, String> {

    Optional<DocumentFileMongoEntity> findByName(String name);

    void deleteByName(String name);
}
