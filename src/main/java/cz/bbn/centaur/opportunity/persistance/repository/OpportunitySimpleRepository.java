package cz.bbn.cerberus.opportunity.persistance.repository;

import cz.bbn.cerberus.opportunity.persistance.entity.OpportunitySimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface OpportunitySimpleRepository
        extends JpaRepository<OpportunitySimpleEntity, Long>, JpaSpecificationExecutor<OpportunitySimpleEntity> {
}
