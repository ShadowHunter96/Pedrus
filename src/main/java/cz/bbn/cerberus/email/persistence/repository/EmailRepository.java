package cz.bbn.cerberus.email.persistence.repository;

import cz.bbn.cerberus.email.persistence.entity.EmailEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface EmailRepository extends JpaRepository<EmailEntity, Long>, JpaSpecificationExecutor<EmailEntity> {
}
