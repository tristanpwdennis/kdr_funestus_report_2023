for lig in ddt permethrin deltamethrin; 
	do 
		for rec in AGAP004707-RD-995F AGAP004707-RD-995S AGAP004707-RD-WT LOC125769886_t4-KDR LOC125769886_t4-WT
			do
			cd $lig'-'$rec
			#echo "running autogrid in" $lig'-'$rec
			#autogrid4 -p $lig'_'$rec.gpf -l $lig'_'$rec.glg
			echo "running vina in" $lig'-'$rec
			~/software/vina_1.2.5_linux_x86_64 --ligand 'ligand_'$lig'.pdbqt' --maps $rec --scoring ad4 --exhaustiveness 32 --out $lig'_'$rec
			cd ..
			done
	done
