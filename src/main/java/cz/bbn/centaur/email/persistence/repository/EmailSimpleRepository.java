package cz.bbn.cerberus.email.persistence.repository;

import cz.bbn.cerberus.email.persistence.entity.EmailSimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface EmailSimpleRepository extends
        JpaRepository<EmailSimpleEntity, Long>, JpaSpecificationExecutor<EmailSimpleEntity> {
}
