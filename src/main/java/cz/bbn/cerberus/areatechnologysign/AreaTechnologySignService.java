package cz.bbn.cerberus.areatechnologysign;

import cz.bbn.cerberus.areatechnologysign.factory.AreaTechnologySignFactory;
import cz.bbn.cerberus.areatechnologysign.persistance.AreaTechnologySignEntity;
import cz.bbn.cerberus.areatechnologysign.persistance.AreaTechnologySignRepository;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class AreaTechnologySignService {

    private final AreaTechnologySignRepository areaTechnologySignRepository;

    public AreaTechnologySignService(AreaTechnologySignRepository areaTechnologySignRepository) {
        this.areaTechnologySignRepository = areaTechnologySignRepository;
    }

    public List<AreaTechnologySignDto> areaTechnologySignDtoList(ObjectType objectType, String objectId){
        List<AreaTechnologySignEntity> areaTechnologySignEntityList =
                areaTechnologySignRepository.findByObjectTypeAndObjectId(objectType, objectId);
        return ConvertEntities.fromEntities(areaTechnologySignEntityList, AreaTechnologySignFactory::fromEntity);
    }

    public void saveAreaTechnologySign(AreaTechnologySignDto dto) throws SystemException {
        AreaTechnologySignEntity entity = new AreaTechnologySignEntity();
        AreaTechnologySignFactory.toEntity(entity, dto);
        if(areaTechnologySignRepository.existsByObjectTypeAndObjectIdAndAreaEntityAndTechnologyEntity(
                entity.getObjectType(), entity.getObjectId(),
                entity.getAreaEntity(), entity.getTechnologyEntity())){
            throw new SystemException(ErrorCode.AREA_TECHNOLOGY_SIGN_EXISTS);
        }
        areaTechnologySignRepository.save(entity);
    }

    public void deleteAreaTechnologySign(Long id){
        areaTechnologySignRepository.deleteById(id);
    }
}
