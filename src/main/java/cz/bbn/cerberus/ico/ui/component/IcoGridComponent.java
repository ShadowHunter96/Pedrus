package cz.bbn.cerberus.ico.ui.component;

import com.vaadin.flow.component.html.Span;
import com.vaadin.flow.data.renderer.ComponentRenderer;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.ItemsAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.ico.dto.IcoDto;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.subject.SubjectComponentOperation;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.subject.ui.component.SubjectAlreadyExistsDialog;
import cz.bbn.cerberus.subject.ui.component.SubjectNewDialog;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class IcoGridComponent extends AppInfiniteGrid<IcoDto> {

    public IcoGridComponent(ItemsAction<IcoDto> itemsAction, AppDialog appDialog,
                            AppInfiniteGrid<SubjectDto> subjectGridComponent,
                            SubjectComponentOperation subjectComponentOperation, boolean local,
                            AppEnv appEnv, ListService listService) {
        super(appEnv, itemsAction);

        addColumn(IcoDto::getIco).setHeader(Transl.get("ICO")).setSortable(true).setKey("ico");
        addColumn(IcoDto::getDic).setHeader(Transl.get("DIC")).setSortable(true).setKey("dic");
        addColumn(IcoDto::getCompanyName).setHeader(Transl.get("Company name")).setSortable(true).setKey("companyName");
        addColumn(IcoDto::getAddress).setHeader(Transl.get("Address")).setSortable(true).setKey("address");
        addColumn(new ComponentRenderer<>(this::getReliable)).setHeader(Transl.get("Reliable"))
                .setSortable(true).setKey("reliable");
        setMinHeight("25em");
        setMinWidth("75em");

        addItemDoubleClickListener(e -> {
            if (appDialog != null) {
                List<String> idList = subjectComponentOperation.getIdByIcoOrDic(
                        e.getItem().getIco(), e.getItem().getDic());
                appDialog.close();
                if (idList.isEmpty()) {
                    SubjectDto subjectDto = new SubjectDto();
                    subjectDto.setLocalSubject(local);
                    IcoDto icoDto = subjectComponentOperation.fillDataFromAres(e.getItem());
                    subjectDto = subjectComponentOperation.setIcoDataToSubject(subjectDto, icoDto);
                    new SubjectNewDialog(subjectGridComponent, subjectComponentOperation, subjectDto,
                            appEnv, listService).open();
                } else {
                    new SubjectAlreadyExistsDialog(subjectGridComponent, subjectComponentOperation,
                            appEnv, listService, idList.get(0)).open();
                }
            }
        });
    }

    public void loadData(List<IcoDto> icoList) {
        this.setItems(icoList);
    }

    private Span getReliable(IcoDto clickedItem) {
        if (clickedItem.getReliable() != null) {
            return new Span(Transl.get(clickedItem.getReliable().name()));
        }
        return new Span();
    }
}
