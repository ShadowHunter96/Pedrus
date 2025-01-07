package cz.bbn.cerberus.enumeration.persistance.repository;

import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationTypeEntity;
import org.springframework.data.jpa.repository.JpaRepository;

public interface EnumerationTypeRepository extends JpaRepository<EnumerationTypeEntity, String> {
}
