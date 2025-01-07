package cz.bbn.cerberus.asset;

import com.lowagie.text.Cell;
import com.lowagie.text.Document;
import com.lowagie.text.Element;
import com.lowagie.text.Font;
import com.lowagie.text.FontFactory;
import com.lowagie.text.HeaderFooter;
import com.lowagie.text.PageSize;
import com.lowagie.text.Paragraph;
import com.lowagie.text.Phrase;
import com.lowagie.text.Rectangle;
import com.lowagie.text.Table;
import com.lowagie.text.pdf.BaseFont;
import com.lowagie.text.pdf.PdfWriter;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.asset.dto.AssetByObjectDto;
import cz.bbn.cerberus.asset.dto.AssetByObjectFilterDto;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.dto.AssetFilterDto;
import cz.bbn.cerberus.asset.dto.AssetSimpleDto;
import cz.bbn.cerberus.asset.factory.AssetFactory;
import cz.bbn.cerberus.asset.persistance.dao.AssetByObjectDao;
import cz.bbn.cerberus.asset.persistance.dao.AssetDao;
import cz.bbn.cerberus.asset.persistance.entity.AssetByObjectEntity;
import cz.bbn.cerberus.asset.persistance.entity.AssetEntity;
import cz.bbn.cerberus.asset.persistance.entity.AssetSimpleEntity;
import cz.bbn.cerberus.asset.persistance.repository.AssetByObjectRepository;
import cz.bbn.cerberus.asset.persistance.repository.AssetRepository;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.translation.Transl;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.FillPatternType;
import org.apache.poi.ss.usermodel.IndexedColors;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.awt.Color;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public class AssetService {

    private final AssetDao assetDao;
    private final AssetRepository assetRepository;
    private final AssetByObjectRepository assetByObjectRepository;
    private final AssetByObjectDao assetByObjectDao;
    private final AppLogService appLogService;
    private final ListService listService;

    public AssetService(AssetDao assetDao, AssetRepository assetRepository,
                        AssetByObjectRepository assetByObjectRepository, AppLogService appLogService,
                        AssetByObjectDao assetByObjectDao, ListService listService) {
        this.assetDao = assetDao;
        this.assetRepository = assetRepository;
        this.assetByObjectRepository = assetByObjectRepository;
        this.appLogService = appLogService;
        this.listService = listService;
        this.assetByObjectDao = assetByObjectDao;
    }

    public List<AssetDto> findAssetList() {
        List<AssetEntity> assetEntityList = assetRepository.findAll();
        return ConvertEntities
                .fromEntities(assetEntityList, AssetFactory::fromEntity);
    }

    public Page<AssetSimpleDto> findAssetPage(AssetFilterDto filter) {
        return assetDao.findAssetPage(filter);
    }

    public AssetDto getAsset(String id) throws SystemException {
        AssetEntity entity = getEntityById(id);
        return AssetFactory.fromEntity(entity);
    }

    public boolean assetExists(String id) {
        return assetRepository.existsById(id);
    }

    @Transactional
    public void saveAsset(AssetDto dto) throws SystemException {
        if (assetRepository.existsById(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        AssetEntity entity = new AssetEntity();
        entity.setId(AppUtils.generateId("AS", getLastSequence()));
        saveAsset(entity, dto);
        appLogService.logInsert(dto, DomainEnum.ASSET_DOMAIN_NAME.getValue());
        listService.reloadAssetDtoList();
    }

    @Transactional
    public void updateAsset(AssetDto dto, AssetDto originalDto) throws SystemException {
        AssetEntity entity = getEntityById(dto.getId());
        saveAsset(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.ASSET_DOMAIN_NAME.getValue());
        listService.reloadAssetDtoList();
    }

    @Transactional
    public void delete(String id) throws SystemException {
        AssetEntity entity = getEntityById(id);
        entity.setDeleted(Boolean.TRUE);
        assetRepository.save(entity);
        appLogService.logDelete(id, DomainEnum.ASSET_DOMAIN_NAME.getValue());
        listService.reloadAssetDtoList();
    }

    public ByteArrayOutputStream getAssetExcel(List<AssetDto> assetDtoList) throws IOException {
        Workbook workbook = null;
        try {
            ByteArrayOutputStream fos = new ByteArrayOutputStream();
            workbook = new XSSFWorkbook();
            Sheet sheet = workbook.createSheet(Transl.get(DomainEnum.ASSET_DOMAIN_NAME.getValue()));
            Row rowHeader = sheet.createRow(0);
            int i = 0;

            CellStyle italicStyle = workbook.createCellStyle();
            italicStyle.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex());
            italicStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            org.apache.poi.ss.usermodel.Font italicFont = workbook.createFont();
            italicFont.setBold(true);
            italicFont.setItalic(true);
            italicStyle.setFont(italicFont);

            CellStyle boldStyle = workbook.createCellStyle();
            boldStyle.setFillForegroundColor(IndexedColors.GREY_25_PERCENT.getIndex());
            boldStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            org.apache.poi.ss.usermodel.Font boldFont = workbook.createFont();
            boldFont.setBold(true);
            boldStyle.setFont(boldFont);

            org.apache.poi.ss.usermodel.Cell idCell = rowHeader.createCell(i++);
            idCell.setCellStyle(italicStyle);
            idCell.setCellValue(Transl.get("Id"));

            org.apache.poi.ss.usermodel.Cell nameCell = rowHeader.createCell(i++);
            nameCell.setCellStyle(boldStyle);
            nameCell.setCellValue(Transl.get("name"));

            org.apache.poi.ss.usermodel.Cell serialNumberCell = rowHeader.createCell(i++);
            serialNumberCell.setCellStyle(boldStyle);
            serialNumberCell.setCellValue(Transl.get("Serial number"));

            org.apache.poi.ss.usermodel.Cell ownerCell = rowHeader.createCell(i++);
            ownerCell.setCellStyle(boldStyle);
            ownerCell.setCellValue(Transl.get("Owner"));

            org.apache.poi.ss.usermodel.Cell typerCell = rowHeader.createCell(i++);
            typerCell.setCellStyle(boldStyle);
            typerCell.setCellValue(Transl.get("Type"));

            org.apache.poi.ss.usermodel.Cell priceCell = rowHeader.createCell(i++);
            priceCell.setCellStyle(boldStyle);
            priceCell.setCellValue(Transl.get("Price"));

            org.apache.poi.ss.usermodel.Cell buyPriceCell = rowHeader.createCell(i++);
            buyPriceCell.setCellStyle(boldStyle);
            buyPriceCell.setCellValue(Transl.get("Buy date"));

            org.apache.poi.ss.usermodel.Cell quaranteeCell = rowHeader.createCell(i++);
            quaranteeCell.setCellStyle(boldStyle);
            quaranteeCell.setCellValue(Transl.get("Quarantee date"));

            org.apache.poi.ss.usermodel.Cell inventoryDateCell = rowHeader.createCell(i++);
            inventoryDateCell.setCellStyle(boldStyle);
            inventoryDateCell.setCellValue(Transl.get("Inventory date"));

            org.apache.poi.ss.usermodel.Cell removalDateCell = rowHeader.createCell(i++);
            removalDateCell.setCellStyle(boldStyle);
            removalDateCell.setCellValue(Transl.get("Removal date"));

            org.apache.poi.ss.usermodel.Cell minorAsset = rowHeader.createCell(i++);
            minorAsset.setCellStyle(boldStyle);
            minorAsset.setCellValue(Transl.get("Minor asset"));

            org.apache.poi.ss.usermodel.Cell latitudeCell = rowHeader.createCell(i++);
            latitudeCell.setCellStyle(boldStyle);
            latitudeCell.setCellValue(Transl.get("Latitude"));

            org.apache.poi.ss.usermodel.Cell longitudeCell = rowHeader.createCell(i++);
            longitudeCell.setCellStyle(boldStyle);
            longitudeCell.setCellValue(Transl.get("Longitude"));

            org.apache.poi.ss.usermodel.Cell descriptionCell = rowHeader.createCell(i++);
            descriptionCell.setCellStyle(boldStyle);
            descriptionCell.setCellValue(Transl.get("Description"));

            org.apache.poi.ss.usermodel.Cell destinationCell = rowHeader.createCell(i++);
            destinationCell.setCellStyle(boldStyle);
            destinationCell.setCellValue(Transl.get("Destination"));

            for (i = 0; i < assetDtoList.size(); i++) {
                AssetDto assetDto = assetDtoList.get(i);
                Row rowAsset = sheet.createRow(i + 1);
                int j = 0;
                rowAsset.createCell(j++).setCellValue(assetDto.getId());
                rowAsset.createCell(j++).setCellValue(assetDto.getName());
                rowAsset.createCell(j++).setCellValue(assetDto.getSerialNumber());
                rowAsset.createCell(j++).setCellValue(assetDto.getType());
                rowAsset.createCell(j++).setCellValue(
                        assetDto.getPrice() != null ? assetDto.getPrice().doubleValue() : 0D);

                rowAsset.createCell(j++).setCellValue(AppUtils.formatDate(assetDto.getBuyDate()));
                rowAsset.createCell(j++).setCellValue(AppUtils.formatDate(assetDto.getQuaranteeDate()));
                rowAsset.createCell(j++).setCellValue(AppUtils.formatDate(assetDto.getInventoryDate()));
                rowAsset.createCell(j++).setCellValue(AppUtils.formatDate(assetDto.getRemovalDate()));

                rowAsset.createCell(j++).setCellValue(assetDto.getLongitude());
                rowAsset.createCell(j++).setCellValue(assetDto.getLatitude());
                rowAsset.createCell(j++).setCellValue(assetDto.getDescription());
                rowAsset.createCell(j++).setCellValue(assetDto.getDestination());
            }
            workbook.write(fos);
            return fos;
        } finally {
            workbook.close();
        }

    }

    public ByteArrayOutputStream getAssetPdf(List<AssetDto> assetDtoList) throws IOException {
        ByteArrayOutputStream fos = new ByteArrayOutputStream();
        Document document = new Document(PageSize.A4, 50, 50, 50, 50);
        PdfWriter writer = PdfWriter.getInstance(document, fos);
        try {

            BaseFont footerHeaderFont = BaseFont.createFont(BaseFont.HELVETICA, "Cp1252", false);
            HeaderFooter footer = new HeaderFooter(
                    new Phrase(Transl.get("Page").concat(": "), new Font(footerHeaderFont)), true);
            footer.setBorder(Rectangle.NO_BORDER);
            footer.setAlignment(Element.ALIGN_CENTER);
            document.setFooter(footer);

            HeaderFooter header = new HeaderFooter(
                    new Phrase(Transl.get("Asset list"), new Font(footerHeaderFont)), false);
            header.setAlignment(Element.ALIGN_CENTER);
            header.setBorder(Rectangle.BOX);
            document.setHeader(header);

            Font tableFont = FontFactory.getFont(FontFactory.HELVETICA, 9);
            Font tableItalicFont = FontFactory.getFont(FontFactory.HELVETICA, 9, Font.BOLDITALIC);
            Font tableBoldFont = FontFactory.getFont(FontFactory.HELVETICA, 9, Font.BOLD);

            document.open();

            for (AssetDto assetDto : assetDtoList) {
                Table table = new Table(4);
                table.setBorderWidth(1);
                table.setPadding(4);
                table.setWidth(100);
                table.setCellsFitPage(true);

                Cell idCell = new Cell(new Phrase(Transl.get("ID").concat(": ").concat(assetDto.getId()),
                        tableItalicFont));
                idCell.setBackgroundColor(Color.LIGHT_GRAY);
                table.addCell(idCell);

                Cell nameCell = new Cell(new Phrase(Transl.get("Name").concat(": ").concat(assetDto.getName()),
                        tableBoldFont));
                nameCell.setColspan(2);
                nameCell.setBackgroundColor(Color.LIGHT_GRAY);
                table.addCell(nameCell);

                Cell serialNumberCell = new Cell(new Phrase(Transl.get("Serial number").concat(": ")
                        .concat(assetDto.getSerialNumber()), tableBoldFont));
                serialNumberCell.setBackgroundColor(Color.LIGHT_GRAY);
                table.addCell(serialNumberCell);

                table.addCell(new Phrase(Transl.get("Type").concat(": ").concat(assetDto.getType()), tableFont));
                table.addCell(new Phrase(Transl.get("Price").concat(": ").concat(assetDto.getPrice().toString()),
                        tableFont));

                table.addCell(new Phrase(Transl.get("Buy date").concat(": ")
                        .concat(AppUtils.formatDate(assetDto.getBuyDate())), tableFont));
                table.addCell(new Phrase(Transl.get("Quarantee date").concat(": ")
                        .concat(AppUtils.formatDate(assetDto.getQuaranteeDate())), tableFont));
                table.addCell(new Phrase(Transl.get("Inventory date").concat(": ")
                        .concat(AppUtils.formatDate(assetDto.getInventoryDate())), tableFont));
                table.addCell(new Phrase(Transl.get("Removal date").concat(": ")
                        .concat(AppUtils.formatDate(assetDto.getRemovalDate())), tableFont));

                String destination = assetDto.getAssetPositionDto() == null ?
                        Transl.get("Own gps coordinates") : assetDto.getAssetPositionDto().getName();
                Cell destinationCell = new Cell(new Phrase(Transl.get("Destination").concat(": ")
                        .concat(destination), tableFont));
                destinationCell.setColspan(2);
                table.addCell(destinationCell);

                table.addCell(new Phrase(Transl.get("Latitude").concat(": ").concat(assetDto.getLatitude()),
                        tableFont));
                table.addCell(new Phrase(Transl.get("Longitude").concat(": ").concat(assetDto.getLongitude()),
                        tableFont));

                Cell descriptionCell = new Cell(new Phrase(Transl.get("Description").concat(": ")
                        .concat(assetDto.getDescription()), tableFont));
                descriptionCell.setColspan(4);
                table.addCell(descriptionCell);

                document.add(table);

                Paragraph p = new Paragraph("");
                p.setSpacingAfter(10);
                document.add(p);
            }

            return fos;
        } finally {
            document.close();
            writer.close();
        }
    }

    private void saveAsset(AssetEntity entity, AssetDto dto) {
        AssetFactory.fillEntity(entity, dto);
        assetRepository.save(entity);
    }

    private AssetEntity getEntityById(String id) throws SystemException {
        return assetRepository.findById(id)
                .orElseThrow(() -> new SystemException(ErrorCode.ASSET_NOT_EXITS, id));
    }

    private AssetByObjectEntity getByObjectEntity(Long id) throws SystemException {
        return assetByObjectRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.ASSET_CONNECTION_NOT_EXISTS, id));
    }

    public List<String> getAssetIdInUseByPosition(String id) {
        return assetRepository.getAssetIdByPositionId(id);
    }

    @Transactional
    public void linkAsset(Set<AssetSimpleDto> selectedItems, String objectId, ObjectType objectType) {
        Set<AssetByObjectEntity> entitySet = new HashSet<>();
        for (AssetSimpleDto assetSimpleDto : selectedItems) {
            AssetByObjectEntity entity = new AssetByObjectEntity();
            entity.setAddedToObjectId(objectId);
            entity.setObjectType(objectType.name());
            AssetSimpleEntity assetEntity = new AssetSimpleEntity();
            AssetFactory.fillEntity(assetEntity, assetSimpleDto);
            entity.setAsset(assetEntity);
            entitySet.add(entity);
            appLogService.log("add asset to object",
                    "object ".concat(objectType.name()).concat(" to id ").concat(objectId), assetSimpleDto.getId());
        }
        assetByObjectRepository.saveAll(entitySet);
    }

    public Page<AssetByObjectDto> findConnectedAssetPage(AssetByObjectFilterDto filter) {
        return assetByObjectDao.findAssetByObjectPage(filter);
    }

    public void unlinkItem(Long id) throws SystemException {
        AssetByObjectEntity entity = getByObjectEntity(id);
        assetByObjectRepository.delete(entity);
        appLogService.log("Unlink", "Asset ".concat(entity.getAsset().getId()).concat(" unlinked from ")
                .concat(entity.getAddedToObjectId()), String.valueOf(entity.getId()));
    }

    public Page<AssetSimpleDto> findAssetExceptOnePage(AssetFilterDto filter, String id) {
        Set<String> existingConnectionId = assetByObjectRepository.getExistingIdConnection(id);
        return assetDao.findAssetPage(filter, id, existingConnectionId);
    }


    private int getLastSequence() {
        String lastId = assetRepository.findLastId(PageRequest.of(0, 1));
        if (lastId != null && !lastId.isEmpty()) {
            int lastSequence = AppUtils.getSequenceFromId(lastId);
            if (lastSequence > 0) {
                return lastSequence;
            }
        }
        return 1;
    }
}
