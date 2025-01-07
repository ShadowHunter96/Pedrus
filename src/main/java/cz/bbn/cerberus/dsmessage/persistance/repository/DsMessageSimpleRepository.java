package cz.bbn.cerberus.dsmessage.persistance.repository;

import cz.bbn.cerberus.dsmessage.persistance.entity.DsMessageSimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;


public interface DsMessageSimpleRepository
        extends JpaRepository<DsMessageSimpleEntity, Long>, JpaSpecificationExecutor<DsMessageSimpleEntity> {

}
