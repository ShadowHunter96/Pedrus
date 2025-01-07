package cz.bbn.cerberus.document.persistance.repository;

import cz.bbn.cerberus.document.persistance.entity.DocumentSimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;


public interface DocumentSimpleRepository
        extends JpaRepository<DocumentSimpleEntity, String>, JpaSpecificationExecutor<DocumentSimpleEntity> {

}
