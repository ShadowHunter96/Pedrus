package cz.bbn.cerberus.assetposition;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.assetposition.dto.AssetPositionDto;
import cz.bbn.cerberus.assetposition.dto.AssetPositionFilterDto;
import cz.bbn.cerberus.assetposition.factory.AssetPositionFactory;
import cz.bbn.cerberus.assetposition.persistance.AssetPositionDao;
import cz.bbn.cerberus.assetposition.persistance.AssetPositionEntity;
import cz.bbn.cerberus.assetposition.persistance.AssetPositionRepository;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class AssetPositionService {

    private final AssetPositionDao assetPositionDao;
    private final AssetPositionRepository assetPositionRepository;
    private final AppLogService appLogService;

    public AssetPositionService(AssetPositionDao assetPositionDao, AssetPositionRepository assetPositionRepository,
                                AppLogService appLogService) {
        this.assetPositionDao = assetPositionDao;
        this.assetPositionRepository = assetPositionRepository;
        this.appLogService = appLogService;
    }

    public List<AssetPositionDto> getAssetPositionDtoList() {
        return ConvertEntities.fromEntities(assetPositionRepository.findAll(), AssetPositionFactory::fromEntity);
    }

    public Page<AssetPositionDto> findAssetPositionDtoPage(AssetPositionFilterDto filter) {
        return assetPositionDao.findAssetPositionPage(filter);
    }

    public AssetPositionDto getAssetPosition(String id) throws SystemException {
        AssetPositionEntity entity = getEntityById(id);
        return AssetPositionFactory.fromEntity(entity);
    }

    @Transactional
    public void savePositionAsset(AssetPositionDto dto) throws SystemException {
        if (assetPositionRepository.existsById(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        AssetPositionEntity entity = new AssetPositionEntity();
        savePositionAsset(entity, dto);
        appLogService.logInsert(dto, DomainEnum.ASSET_POSITION_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updatePositionAsset(AssetPositionDto dto, AssetPositionDto originalDto) throws SystemException {
        AssetPositionEntity entity = getEntityById(dto.getId());
        savePositionAsset(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.ASSET_POSITION_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void delete(String id) throws SystemException {
        AssetPositionEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        assetPositionRepository.save(entity);
    }

    private void savePositionAsset(AssetPositionEntity entity, AssetPositionDto dto) {
        AssetPositionFactory.fillEntity(entity, dto);
        assetPositionRepository.save(entity);
    }

    private AssetPositionEntity getEntityById(String id) throws SystemException {
        return assetPositionRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.ASSET_POSITION_NOT_EXITS, id));
    }
}
