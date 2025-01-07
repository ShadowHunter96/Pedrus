package cz.bbn.cerberus.labelsubject.ui.component;

import com.vaadin.flow.component.grid.Grid;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.provider.ListDataProvider;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.translation.Transl;


public class LabelSubjectRowDetail extends HorizontalLayout {

    public void setRow(LabelSubjectDto dto){
        this.removeAll();
        this.setHeight("10em");
        this.setAlignItems(Alignment.CENTER);
        Grid<ItemDto> grid = new Grid<>();
        grid.setMaxWidth("50em");
        grid.setMaxHeight("10em");
        grid.addColumn(ItemDto::getName).setHeader(Transl.get("Value"));
        ListDataProvider<ItemDto> dataProvider = new ListDataProvider<>(dto.getTableSet());
        grid.setDataProvider(dataProvider);
        this.add(grid);
    }
}
