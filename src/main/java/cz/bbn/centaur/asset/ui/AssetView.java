package cz.bbn.cerberus.asset.ui;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.router.Route;
import com.vaadin.flow.server.StreamResource;
import cz.bbn.cerberus.asset.AssetComponentOperation;
import cz.bbn.cerberus.asset.AssetService;
import cz.bbn.cerberus.asset.dto.AssetDto;
import cz.bbn.cerberus.asset.ui.component.AssetFilterComponent;
import cz.bbn.cerberus.asset.ui.component.AssetGridComponent;
import cz.bbn.cerberus.commons.RobotFrameworkVariables;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppView;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.appcard.AppCardGridComponent;
import cz.bbn.cerberus.commons.component.ui.slidetab.NewEntityButtonsComponent;
import cz.bbn.cerberus.commons.entitynew.EntityNewComponentOperation;
import cz.bbn.cerberus.commons.entitynew.EntityNewType;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.document.dto.DocumentFileDto;
import cz.bbn.cerberus.mainlayout.ui.MainLayout;
import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.permission.Authorize;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.vaadin.olli.FileDownloadWrapper;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.time.LocalDateTime;
import java.util.List;

@Route(value = AssetView.ROUTE, layout = MainLayout.class)
@Authorize(Permission.ASSET_VIEW)
@Slf4j
public class AssetView extends AppView {

    public static final String ROUTE = "asset-list";

    private final AppEnv appEnv;
    private final AssetService assetService;
    private final AssetComponentOperation assetComponentOperation;
    private final EntityNewComponentOperation entityNewComponentOperation;

    public AssetView(AppEnv appEnv, AssetService assetService, AssetComponentOperation assetComponentOperation,
                     EntityNewComponentOperation entityNewComponentOperation) {
        this.appEnv = appEnv;
        this.assetService = assetService;
        this.assetComponentOperation = assetComponentOperation;
        this.entityNewComponentOperation = entityNewComponentOperation;
        initView();
    }

    private void initView() {
        removeAll();
        setSizeFull();

        Button search = VaadinComponents.getSearchButton();
        AssetFilterComponent assetFilterComponent = new AssetFilterComponent(search);

        AssetGridComponent grid = new AssetGridComponent(assetComponentOperation.getDeleteAction(), appEnv,
                assetComponentOperation.getItemsAction(assetFilterComponent), assetComponentOperation);

        Button pdfButton = VaadinComponents.getButton(Transl.get("PDF"), VaadinIcon.FILE_TEXT.create());
        StreamResource pdfResource = new StreamResource(
                Transl.get("Assets").concat("-").concat(LocalDateTime.now().toString()).concat(".pdf"),
                () -> getPdfFileDto().getFileData());

        FileDownloadWrapper buttonPdfWrapper = new FileDownloadWrapper(pdfResource);
        buttonPdfWrapper.wrapComponent(pdfButton);

        Button excelButton = VaadinComponents.getButton(Transl.get("Excel"), VaadinIcon.FILE_TEXT.create());
        StreamResource excelResource = new StreamResource(
                Transl.get("Assets").concat("-").concat(LocalDateTime.now().toString()).concat(".xlsx"),
                () -> getExcelFileDto().getFileData());

        FileDownloadWrapper buttonExcelWrapper = new FileDownloadWrapper(excelResource);
        buttonExcelWrapper.wrapComponent(excelButton);

        AppCardGridComponent card = new AppCardGridComponent(Transl.get("Asset list"),
                Permission.ASSET_EDIT, Transl.get("Add asset"),
                entityNewComponentOperation.getNewAssetEvent(grid, null),
                entityNewComponentOperation, NoteTypeEnum.ANY, ObjectType.ANY,
                buttonPdfWrapper, buttonExcelWrapper
        );

        card.setId(RobotFrameworkVariables.ASSET_VIEW_CARD_ID.getValue());
        card.add(assetFilterComponent);
        card.add(grid);

        card.addNewEntitySlideTab(new NewEntityButtonsComponent(entityNewComponentOperation, EntityNewType.BACKOFFICE));

        add(card);
        grid.loadData();
        search.addClickListener(buttonClickEvent -> grid.loadData());
    }

    private DocumentFileDto getExcelFileDto() {
        try {
            List<AssetDto> assetDto = assetService.findAssetList();
            ByteArrayOutputStream outputStream = assetService.getAssetExcel(assetDto);
            DocumentFileDto documentFileDto = new DocumentFileDto();
            documentFileDto.setFileData(new ByteArrayInputStream(outputStream.toByteArray()));
            return documentFileDto;
        } catch (IOException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
        return null;
    }

    private DocumentFileDto getPdfFileDto() {
        try {
            List<AssetDto> assetDto = assetService.findAssetList();
            ByteArrayOutputStream outputStream = assetService.getAssetPdf(assetDto);
            DocumentFileDto documentFileDto = new DocumentFileDto();
            documentFileDto.setFileData(new ByteArrayInputStream(outputStream.toByteArray()));
            return documentFileDto;
        } catch (IOException ex) {
            log.error(TextValues.SYSTEM_EXCEPTION, ex);
            ErrorNotification.show(ex, appEnv);
        }
        return null;
    }
}
